# The main "driver" class for a command processor. Other parts of the 
# command class and debugger command objects are pulled in from here.

require 'linecache'
require 'set'
require 'pathname'  # For cleanpath

%w(default display eventbuf eval load_cmds location frame hook msg 
   validate).each do
  |mod_str|
  require_relative mod_str
end
require_relative '../app/brkptmgr'

class Debugger
  class CmdProcessor

    # SEE ALSO attr's in require_relative's of loop above.

    attr_reader   :core            # Debugger core object
    attr_reader   :current_command # Current command getting run, a String.
    attr_reader   :dbgr            # Debugger instance (via
                                   # Debugger::Core instance)
    attr_accessor :different_pos   # Same type as settings[:different] 
                                   # this is the temporary value for the
                                   # next stop while settings is the default
                                   # value to use.
    attr_accessor :leave_cmd_loop  # Commands set this to signal to leave
                                   # the command loop (which often continues to 
                                   # run the debugged program). 
    attr_accessor :line_no         # Last line shown in "list" command
    attr_accessor :pass_exception  # Pass an exception back 
    attr_accessor :next_level      # Fixnum. frame.stack_size has to
                                   # be <= than this.  If next'ing,
                                   # this will be > 0.
    attr_accessor :next_thread     # Thread. If non-nil then in
                                   # stepping the thread has to be
                                   # this thread.
    attr_reader   :settings        # Hash[:symbol] of command
                                   # processor settings

    # The following are used in to force stopping at a different line
    # number. FIXME: could generalize to a position object.
    attr_accessor :last_pos       # Last position. 4-Tuple: of
                                  # [location, container, stack_size, current_thread]


    unless defined?(EVENT2ICON)
      # We use event icons in printing locations.
      EVENT2ICON = {
        'brkpt'          => 'xx',
        'c-call'         => 'C>',
        'c-return'       => '<C',
        'call'           => '->',
        'class'          => '::',
        'coverage'       => '[]',
        'debugger-call'  => ':o',
        'end'            => '-|',
        'line'           => '--',
        'raise'          => '!!',
        'return'         => '<-',
        'switch'         => 'sw',
        'trace-var'      => '$V',
        'unknown'        => '?!',
        'vm'             => 'VM',
        'vm-insn'        => '..',
      } 
      # These events are important enough event that we always want to
      # stop on them.
      UNMASKABLE_EVENTS = Set.new(['end', 'raise', 'unknown'])
    end

    def initialize(core, settings={})
      @core            = core
      @dbgr            = core.dbgr
      @hidelevels      = {}
      @last_command    = nil
      @last_pos        = [nil, nil, nil, nil]
      @next_level      = 32000
      @next_thread     = nil

      start_cmds       = settings.delete(:start_cmds)
      start_file       = settings.delete(:start_file)

      @settings        = settings.merge(DEFAULT_SETTINGS)
      @different_pos   = @settings[:different]

      # FIXME: Rework using a general "set substitute file" command and
      # a global default profile which gets read.
      prelude_file = File.expand_path(File.join(File.dirname(__FILE__), 
                                                %w(.. data prelude.rb)))
      LineCache::cache(prelude_file)
      LineCache::remap_file('<internal:prelude>', prelude_file)

      # Start with empty thread and frame info.
      frame_teardown 

      # Run initialization routines for each of the "submodule"s.
      # load_cmds has to come first.
      %w(load_cmds breakpoint display eventbuf frame running validate
         ).each do |submod|
        self.send("#{submod}_initialize")
      end
      hook_initialize(commands)

      # FIXME: run start file and start commands.
    end

    def canonic_container(container)
      [container[0], canonic_file(container[1])]
    end

    def canonic_file(filename)
      # For now we want resolved filenames 
      @settings[:basename] ? File.basename(filename) : 
        # Cache this?
        Pathname.new(filename).cleanpath.to_s
    end

    # Check that we meed the criteria that cmd specifies it needs
    def ok_for_running(cmd, name, nargs)
      # TODO check execution_set against execution status.
      # Check we have frame is not null
      min_args = cmd.class.const_get(:MIN_ARGS)
      if nargs < min_args
        errmsg(("Command '%s' needs at least %d argument(s); " + 
                "got %d.") % [name, min_args, nargs])
        return false
      end
      max_args = cmd.class.const_get(:MAX_ARGS)
      if max_args and nargs > max_args
        errmsg(("Command '%s' needs at most %d argument(s); " + 
                "got %d.") % [name, max_args, nargs])
        return false
      end
      # if cmd.class.const_get(:NEED_RUNNING) && !...
      #   errmsg "Command '%s' requires a running program." % name
      #   return false
      # end

      if cmd.class.const_get(:NEED_STACK) && !@frame
        errmsg "Command '%s' requires a running stack frame." % name
        return false
      end

      return true
    end

    # Run one debugger command. True is returned if we want to quit.
    def process_command_and_quit?()
      intf = @dbgr.intf
      return true if intf[-1].input.eof? && intf.size == 1
      while !intf[-1].input.eof? || intf.size > 1
        begin
          @current_command = read_command().strip
          if @current_command.empty? 
            if @last_command && intf[-1].interactive?
              @current_command = @last_command 
            else
              next
            end
          end
          next if @current_command[0..0] == '#' # Skip comment lines
          break
        rescue IOError, Errno::EPIPE
          if @dbgr.intf.size > 1
            @dbgr.intf.pop 
            @last_command = nil
            print_location
          else
            msg "EOF - Leaving"
            ## FIXME: think of something better.
            quit('quit!')
            return true
          end
        end
      end
      run_command(@current_command)
    end

    # This is the main entry point.
    def process_commands(frame)

      frame_setup(frame)

      @unconditional_prehooks.run
      return if !breakpoint? && stepping_skip?

      @leave_cmd_loop = false
      print_location unless @settings[:traceprint]
      if 'trace-var' == @core.event 
        msg "Note: we are stopped *after* the above location."
      end

      @eventbuf.add_mark if @settings[:tracebuffer]

      @cmdloop_prehooks.run
      while not @leave_cmd_loop do
        begin
          break if process_command_and_quit?()
        rescue SystemExit
          @dbgr.stop
          raise
        rescue Exception => exc
          errmsg("Internal debugger error: #{exc}.inspect")
          exception_dump(exc, @settings[:debugexcept], $!.backtrace)
        end
      end
      @cmdloop_posthooks.run
    end

    # Run current_command, a String. @last_command is set after the
    # command is run if it is a command.
    def run_command(current_command)
      eval_command = 
        if current_command[0..0] == '!'
          current_command[0] = ''
        else
          false
        end
      unless eval_command
        args = current_command.split
        return false if args.size == 0
        cmd_name = args[0]
        cmd_name = @aliases[cmd_name] if @aliases.member?(cmd_name)
        if @commands.member?(cmd_name)
          cmd = @commands[cmd_name]
          if ok_for_running(cmd, cmd_name, args.size-1)
            cmd.run(args) 
            @last_command = current_command
          end
          return false
        end
      end

      # Eval anything that's not a command or has been
      # requested to be eval'd
      if settings[:autoeval] || eval_command
        msg 'D=> ' + debug_eval(current_command).inspect
      else
        undefined_command(cmd_name)
      end
      return false
    end

    # Error message when a command doesn't exist
    def undefined_command(cmd_name)
      errmsg('Undefined command: "%s". Try "help".' % cmd_name)
    end

    # FIXME: Allow access to both Debugger::CmdProcessor and Debugger
    # for index [] and []=.
    # If there is a Debugger::CmdProcessor setting that would take precidence.
    # def settings
    #   @settings.merge(@dbgr.settings) # wrong because this doesn't allow []=
    # end
  end
end

if __FILE__ == $0
  $0 = 'foo' # So we don't get here again
  require_relative '../lib/rbdbgr'
  dbg =  Debugger.new(:nx => true)
  dbg.core.processor.msg('I am main')
  dbg.core.processor.errmsg('Whoa!')
  cmds = dbg.core.processor.commands
  p dbg.core.processor.aliases
  cmd_name, cmd_obj = cmds.first
  puts cmd_obj.class.const_get(:HELP)
  puts cmd_obj.class.const_get(:SHORT_HELP)

  if ARGV.size > 0
    dbg.core.processor.msg('Enter "q" to quit')
    dbg.proc_process_commands
  else
    $input = []
    class << dbg.core.processor
      def read_command
        $input.shift
      end
    end
    $input = ['1+2']
    dbg.core.processor.process_command_and_quit?
    $input = ['!s = 5']  # ! means eval line 
    dbg.core.processor.process_command_and_quit?
  end
end
