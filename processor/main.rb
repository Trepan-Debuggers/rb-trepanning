# The main "driver" class for a command processor. Other parts of the 
# command class and debugger command objects are pulled in from here.

require 'linecache'
require 'set'
require 'pathname'  # For cleanpath

%w(default location frame hook msg validate).each do |mod_str|
    require_relative mod_str
end
require_relative %w(.. app brkptmgr)

class Debugger
  class CmdProcessor

    # SEE ALSO attr's in require_relative's of loop above.

    attr_reader   :aliases         # Hash[String] of command names
                                   # indexed by alias name
    attr_reader   :core            # Debugger core object
    attr_reader   :commands        # Hash[String] of command objects
                                   # indexed by name
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
    attr_accessor :next_level      # Fixnum. frame.stack_size has to
                                   # be <= than this.  If next'ing,
                                   # this will be > 0.
    attr_accessor :next_thread     # Thread. If non-nil then in
                                   # stepping the thread has to be
                                   # this thread.
    attr_reader   :settings        # Hash[:symbol] of command
                                   # processor settings
    attr_accessor :stop_events     # Set or nil. If not nil, only
                                   # events in this set will be
                                   # considered for stopping. This is
                                   # like core.step_events (which
                                   # could be used instead), but it is
                                   # a set of event names rather than
                                   # a bitmask and it is intended to
                                   # be more temporarily changed via
                                   # "step>" or "step!" commands.
                                  

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
      @settings        = settings.merge(DEFAULT_SETTINGS)
      @different_pos   = @settings[:different]
      @remap_container = {}  # See location.rb
      @stop_events     = nil

      # FIXME: Rework using a general "set substitute file" command and
      # a global default profile which gets read.
      prelude_file = File.expand_path(File.join(File.dirname(__FILE__), 
                                                %w(.. data prelude.rb)))
      LineCache::cache(prelude_file)
      LineCache::remap_file('<internal:prelude>', prelude_file)

      # Start with empty thread and frame info.
      frame_teardown 

      # Load up debugger commands. Sets @commands, @aliases
      cmd_dir = File.expand_path(File.join(File.dirname(__FILE__),
                                           'command'))
      load_debugger_commands(cmd_dir)

      breakpoint_initialize
      hook_initialize(commands)
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

    def debug_eval(str)
      begin
        b = @frame.binding if @frame 
        b ||= binding
        eval(str, b)
      rescue StandardError, ScriptError => e
        exception_dump(e, settings[:stack_trace_on_error], $!.backtrace)
      end
    end

    def debug_eval_no_errmsg(str)
      begin
        b = @frame.binding if @frame 
        b ||= binding
        eval(str, b)
      rescue StandardError, ScriptError => e
        nil
      end
    end


    def exception_dump(e, stack_trace, backtrace)
      str = "#{e.class} Exception: #{e.message}"
      if stack_trace
        str += "\n" + backtrace.map{|l| "\t#{l}"}.join("\n") rescue nil
      end
      errmsg str
      # throw :debug_error
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
        errmsg "Command '%s' requires at running stack frame." % name
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
          last_command = read_command().strip
          if last_command.empty? && @last_command && intf[-1].interactive?
            last_command = @last_command 
          end
          next if last_command[0..0] == '#' # Skip comment lines
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
      args = last_command.split
      return false if args.size == 0
      cmd_name = args[0]
      cmd_name = @aliases[cmd_name] if @aliases.member?(cmd_name)
      if @commands.member?(cmd_name)
        cmd = @commands[cmd_name]
        if ok_for_running(cmd, cmd_name, args.size-1)
          cmd.run(args) 
          @last_command = last_command
        end
        return false
      end
        
      # Eval anything that's not a command.
      if settings[:autoeval]
        msg debug_eval(last_command) 
      else
        undefined_command(cmd_name)
      end
      return false
    end

    def stepping_skip?

      return true if @core.step_count < 0
      if @settings[:'debugskip']
        puts "diff: #{@different_pos}, event : #{@core.event}, #{@stop_events.inspect}" 
        puts "nl  : #{@next_level},    ssize : #{@stack_size}" 
        puts "nt  : #{@next_thread},   thread: #{Thread.current}" 
      end

      # I think these events are important enough event that we always want
      # to stop on them.
      # return false if UNMASKABLE_EVENTS.member?(@core.event)

      frame = @frame
      while 'CFUNC' == frame.type && frame do
        frame = frame.prev
      end
      return true if 
        !frame || (@next_level < frame.stack_size &&
                   Thread.current == @next_thread)

      new_pos = [@frame.source_container, frame_line,
                 @stack_size, @current_thread]

      skip_val = @stop_events && !@stop_events.member?(@core.event)

      if @settings[:'debugskip']
        puts "skip: #{skip_val.inspect}, last: #{@last_pos}, new: #{new_pos}" 
      end

      skip_val = (@last_pos == new_pos) && @different_pos unless skip_val
      @last_pos = new_pos if !@stop_events || @stop_events.member?(@core.event)

      unless skip_val
        # Set up the default values for the
        # next time we consider skipping.
        @different_pos = @settings[:different]
        @stop_events   = nil
      end

      return skip_val
    end

    # This is the main entry point.
    def process_commands(frame)

      frame_setup(frame)

      @unconditional_prehooks.run
      return if !breakpoint? && stepping_skip?

      @leave_cmd_loop = false
      print_location unless @settings[:trace]

      @cmdloop_prehooks.run

      while not @leave_cmd_loop do
        begin
          break if process_command_and_quit?()
        rescue SystemExit
          @dbgr.stop
          raise
        rescue Exception => e
          errmsg("Internal debugger error!")
          exception_dump(e, @settings[:debugexcept], $!.backtrace)
        end
      end
    end

    # Loads in debugger commands by require'ing each ruby file in the
    # 'command' directory. Then a new instance of each class of the 
    # form Debugger::xxCommand is added to @commands and that array
    # is returned.
    def load_debugger_commands(cmd_dir)
      Dir.glob(File.join(cmd_dir, '*.rb')).each do |rb| 
        require rb
      end if File.directory?(cmd_dir)
      # Instantiate each Command class found by the above require(s).
      @commands = {}
      @aliases = {}
      Debugger::Command.constants.grep(/.Command$/).each do |command|
        # Note: there is probably a non-eval way to instantiate the
        # command, but I don't know it. And eval works.
        new_cmd = "Debugger::Command::#{command}.new(self)"
        cmd = self.instance_eval(new_cmd)

        # Add to list of commands and aliases.
        cc = cmd.class
        cmd_name = cc.const_get(:NAME)
        if cc.constants.member?(:ALIASES)
          aliases= cc.const_get(:ALIASES)  
          aliases.each {|a| @aliases[a] = cmd_name}
        end
        @commands[cmd_name] = cmd
      end
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
  require_relative %w(.. lib rbdbgr)
  dbg =  Debugger.new
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
  end
end
