# The main "driver" class for a command processor. Other parts of the 
# command class and debugger command objects are pulled in from here.

require 'linecache'
require 'set'
require 'pathname'  # For cleanpath

require_relative 'default'  # Command Processor default settings
require_relative 'frame'
require_relative 'msg'
require_relative 'validate'
require_relative %w(.. lib brkptmgr)

class Debugger
  class CmdProcessor
    attr_reader   :aliases        # Hash[String] of command names
                                  # indexed by alias name
    attr_reader   :brkpt          # Breakpoint. If we are stopped at a
                                  # breakpoint this is the one we
                                  # found.  (There may be other
                                  # breakponts that would have caused a stop
                                  # as well; this is just one of them).
                                  # If no breakpoint stop this is nil.
    attr_reader   :brkpts         # BreakpointManager. 
    attr_reader   :core           # Debugger core object
    attr_reader   :commands       # Hash[String] of command objects
                                  # indexed by name
    attr_reader   :dbgr           # Debugger instance (via
                                  # Debugger::Core instance)
    attr_accessor :different_pos  # Same type as settings[:different] 
                                  # this is the temporary value for the
                                  # next stop while settings is the default
                                  # value to use.
    attr_accessor :leave_cmd_loop # Commands set this to signal to leave
                                  # the command loop (which often continues to 
                                  # run the debugged program). 
    attr_accessor :line_no        # Last line shown in "list" command
    attr_accessor :next_level     # Fixnum. frame.stack_size has to be <= than this.
                                  # If next'ing, this will be > 0.
    attr_accessor :next_thread    # If non-nil then in stepping the thread has to be 
                                  # this thread.
    attr_reader   :settings       # Hash[:symbol] of command processor
                                  # settings
    attr_accessor :stop_events    # Set or nil. If not nil, only
                                  # events in this set will be
                                  # considered for stopping. This is
                                  # like core.step_events (which could
                                  # be used instead), but it is a set
                                  # of event names rather than a
                                  # bitmask and it is intended to be
                                  # more temporarily changed via "step>" or
                                  # "step!" commands.
                                  

    # The following are used in to force stopping at a different line
    # number. FIXME: could generalize to a position object.
    attr_accessor :last_pos       # Last position. 4-Tuple: of
                                  # [location, container, stack_size, current_thread]


    unless defined?(EVENT2ICON)
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
      @brkpts         = BreakpointMgr.new
      @brkpt          = nil
      @core           = core
      @dbgr           = core.dbgr
      @hidelevels     = {}
      @last_command   = nil
      @last_pos       = [nil, nil, nil, nil]
      @next_level     = 32000
      @next_thread    = nil
      @settings       = settings.merge(DEFAULT_SETTINGS)
      @different_pos  = @settings[:different]
      @stop_events    = nil

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
      if cmd.class.const_get(:NEED_STACK) && !@frame
        errmsg "Command '%s' requires at running stack frame." % name
        return false
      end
        
      return true
    end

    # Get line +line_number+ from file named +filename+. Return "\n"
    # there was a problem. Leaking blanks are stripped off.
    def line_at(filename, line_number) # :nodoc:
      @reload_on_change=nil unless defined?(@reload_on_change)
      line = LineCache::getline(filename, line_number, @reload_on_change)
      return "\n" unless line
      return line.gsub(/^\s+/, '').chomp
    end

    def print_location
      text      = nil
      container = frame_file(@frame, false)
      ev        = if @core.event.nil? || @frame_index != 0 
                    '  ' 
                  else
                    (EVENT2ICON[@core.event] || @core.event)
                  end
      @line_no  = frame_line
      loc       = "#{canonic_file(container)}:#{line_no}"
      if @frame.source_container[0] != 'file'
        frame = @frame
        via = loc
        while frame.source_container[0] != 'file' and frame.prev do
          puts "++ #{frame}"
          frame     = frame.prev
        end
        if frame.source_container[0] == 'file'
          container = frame_file(frame, false)
          @line_no  = frame.source_location[0]
          loc      += " via #{canonic_file(container)}:#{@line_no}"
          text      = line_at(container, @line_no)
        end
      else
        map_file, map_line = LineCache::map_file_line(container, @line_no)
        if [container, @line_no] != [map_file, map_line]
          loc += " remapped #{canonic_file(map_file)}:#{map_line}"
        end
        
        text  = line_at(container, @line_no)
      end
      message = "#{ev} (#{loc})"
      if text && !text.strip.empty?
        message += "\n#{text}" 
        @line_no -= 1
      end
      msg message
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

    def breakpoint?
      @brkpt = @brkpts.find(@frame.iseq, @frame.pc_offset, @frame.binding)
      @brkpts.delete_by_brkpt(@brkpt) if @brkpt && @brkpt.temp?
      return !!@brkpt
    end

    def stepping_skip?

      return true if @core.step_count < 0
      if @settings[:'debug-skip']
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

      new_pos = [@frame.source_container, @frame.source_location, 
                 @stack_size, @current_thread]

      skip_val = @stop_events && !@stop_events.member?(@core.event)

      if @settings[:'debug-skip']
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
      return if !breakpoint? && stepping_skip?

      print_location

      @leave_cmd_loop = false

      # FIXME: do this as a pre-command hook -- which means *writing*
      # pre-command hooks.
      irb_cmd = @commands['irb']
      irb_cmd.run(['irb']) if @settings[:autoirb] && irb_cmd

      while not @leave_cmd_loop do
        begin
          break if process_command_and_quit?()
        rescue Exception => e
          errmsg("INTERNAL DEBUGGER ERROR!")
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
  require_relative %w(.. rbdbgr)
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
