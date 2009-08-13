# The main "driver" class for a command processor. Other parts of the 
# command class and debugger command objects are pulled in from here.

require_relative 'default'  # Command Processor default settings
require_relative 'frame'
require_relative 'msg'
require_relative 'validate'

class Debugger
  class CmdProcessor
    attr_reader   :aliases      # Hash[String] of command names
                                # indexed by alias name
    attr_reader   :dbgr         # Debugger instance (via
                                # Debugger::Core instance)
    attr_reader   :commands     # Hash[String] of command objects
                                # indexed by name
    attr_reader   :settings     # Hash[:symbol] of command processor
                                # settings

    def initialize(core, settings={})
      @core           = core
      @dbgr           = core.dbgr
      @event          = nil
      @settings       = settings.merge(DEFAULT_SETTINGS)

      # Start with empty thread and frame info.
      frame_teardown 

      # Load up debugger commands. Sets @commands, @aliases
      cmd_dir = File.expand_path(File.join(File.dirname(__FILE__),
                                           'command'))
      load_debugger_commands(cmd_dir)
    end

    def debug_eval(str)
      begin
        b = @frame.binding if @frame 
        b ||= binding
        eval(str, b)
      rescue StandardError, ScriptError => e
        exception_dump(e, settings[:stack_trace_on_error], b)
      end
    end

    def exception_dump(e, stack_trace, b=nil)
      if stack_trace
        at = eval("caller(2)", b)
        str = "%s:%s\n" % [at.shift, e.to_s.sub(/\(eval\):1:(in `.*?':)?/, '')]
        str += at.map{|s| "\tfrom %s" % [s]}.join("\n")
      else
        str = "#{e.class} Exception: #{e.message}"
      end
      errmsg str
#         throw :debug_error
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
      return true
    end

    def print_location
      msg "(#{@frame.source_container[1]}:#{@frame.source_location[0]})"
    end

    # Run one debugger command. True is returned if we want to quit.
    def process_command_and_quit?()
      str = read_command()
      return false unless str
      args = str.split
      return false if args.size == 0
      cmd_name = args[0]
      cmd_name = @aliases[cmd_name] if @aliases.member?(cmd_name)
      if @commands.member?(cmd_name)
        cmd = @commands[cmd_name]
        if ok_for_running(cmd, cmd_name, args.size-1)
          return cmd.run(args) 
        else
          return false
        end
      else
        # Warning: the next line is going away...
        return true if !str || 'q' == str.strip
      end
        
      # Eval anything that's not a command.
      if settings[:autoeval]
        msg debug_eval(str) 
      else
        undefined_command(cmd_name)
      end
      return false
    end

    # This is the main entry point.
    def process_commands(frame)

      frame_setup(frame, Thread.current)
      print_location

      leave_loop = false
      while not leave_loop do
          leave_loop = process_command_and_quit?()
          # Might have other stuff here.
        end
    rescue IOError, Errno::EPIPE
    # rescue Exception => e
    #   errmsg("INTERNAL ERROR!!!")
    #   b = @frame.binding if @frame 
    #   exception_dump(e, false, b)
    #   frame_teardown
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
  require_relative File.join(%w(.. rbdbgr))
  dbg =  Debugger.new
  dbg.core.processor.msg('I am main')
  dbg.core.processor.errmsg('Whoa!')
  cmds = dbg.core.processor.instance_variable_get('@commands')
  p dbg.core.processor.instance_variable_get('@aliases')
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
