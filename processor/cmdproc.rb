class Debugger
  class CmdProcessor
    attr_reader :aliases  # Hash of command names indexed by alias name
    attr_reader :commands # Hash of command objects indexed by name

    def initialize(core)
      @event    = nil
      @frame    = nil
      @prompt   = '(rdbgr): '
      @core     = core
      # Load up debugger commands. Sets @commands, @aliases
      load_debugger_commands 
    end

    def debug_eval(str)
      begin
        b = @frame.binding if @frame 
        b ||= binding
        eval(str, b)
      rescue StandardError, ScriptError => e
#         if Command.settings[:stack_trace_on_error]
#           at = eval("caller(1)", b)
#           print "%s:%s\n", at.shift, e.to_s.sub(/\(eval\):1:(in `.*?':)?/, '')
#           for i in at
#             print "\tfrom %s\n", i
#           end
#         else
#           print "#{e.class} Exception: #{e.message}\n"
#         end
#         throw :debug_error
        errmsg "#{e.class} Exception: #{e.message}\n"
      end
    end

    def errmsg(message)
      puts "Error: #{message}"
    end

    # Like cmdfns.get_an_int(), but if there's a stack frame use that
    # in evaluation.
    def get_an_int(arg, opts={})
      ret_value = get_int_noerr(arg)
      if ret_value
        if opts[:msg_on_error]
          errmsg(opts[:msg_on_error])
        else
          errmsg("Expecting an integer, got: #{arg}.")
        end
        return nil
        if opts[:min_value] and ret_value < opts[:min_value]
          errmsg("Expecting integer value to be at least %d; got %d.",
                 opts[min_value], ret_value)
          return nil
        elsif opts[:max_value] and ret_value > opts[:max_value]
            errmsg("Expecting integer value to be at most %d; got %d.",
                   opts[:min_value], ret_value)
            return nil
        end
      end
      return ret_value
    end

    unless defined?(DEFAULT_GET_INT_OPTS)
      DEFAULT_GET_INT_OPTS = {
        :min_value => 0, :default => 1, :cmdname => nil, :at_most => nil}
    end

    # If no argument use the default. If arg is a an integer between
    # least min_value and at_most, use that. Otherwise report an error.
    # If there's a stack frame use that in evaluation.
    def get_int(arg, opts={})
      
      return default unless arg
      opts = DEFAULT_GET_INT_OPTS.merge(opts)
      val = arg ? get_int_noerr(arg) : opts[:default]
      unless val
        if opts[:cmdname]
          errmsg(("Command '%s' expects an integer; " +
                  "got: %s.") % [opts[:cmdname], arg])
        else
          errmsg('Expecting a positive integer, got: %s' % arg)
        end
        return nil
      end
      
      if val < opts[:min_value]
        if cmdname
          errmsg(("Command '%s' expects an integer at least" +
                  ' %d; got: %d.') %
                 [cmdname, opts[:min_value], opts[:default]])
        else
          errmsg(("Expecting a positive integer at least" +
                  ' %d; got: %d') %
                 [opts[:min_value], opts[:default]])
        end
        return nil
      elsif opts[:at_most] and val > opts[:at_most]
        if opts[:cmdname]
          errmsg(("Command '%s' expects an integer at most" +
                  ' %d; got: %d.') %
                 [opts[:cmdname], opts[:at_most], val])
        else
          errmsg(("Expecting an integer at most %d; got: %d") %
                 [opts[:at_most], val])
        end
      end
      return val
    end

    # Eval arg and it is an integer return the value. Otherwise
    # return nil
    def get_int_noerr(arg)
      b = @frame ? @frame.binding : nil
      begin
        val = Integer(eval(arg, b))
      rescue 
        return nil
      end
    end

    def msg(message)
      puts message
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
        return @commands[cmd_name].run(args) 
      else
        # Warning: the next line is going away...
        return true if !str || 'q' == str.strip
      end
        
        # Eval anything that's not a command.
      puts debug_eval(str)
      return false
    end

    def process_commands(frame=nil)
      @frame = frame
      leave_loop = false
        while not leave_loop do
          leave_loop = process_command_and_quit?()
          # Might have other stuff here.
        end
    rescue IOError, Errno::EPIPE
    rescue Exception
      puts "INTERNAL ERROR!!! #{$!}\n" rescue nil
    end

    def read_command()
      require 'readline'
      Readline.readline(@prompt)
    end

    # Loads in debugger commands by require'ing each ruby file in the
    # 'command' directory. Then a new instance of each class of the 
    # form Debugger::xxCommand is added to @commands and that array
    # is returned.
    def load_debugger_commands
      cmd_dir = File.expand_path(File.join(File.dirname(__FILE__),
                                           'command'))
      Dir.chdir(cmd_dir) do
        # Note: require_relative doesn't seem to pick up the above
        # chdir.
        Dir.glob('*.rb').each { |rb| require File.join(cmd_dir, rb) }
      end if File.directory?(cmd_dir)
      # Instantiate each Command class found by the above require(s).
      @commands = {}
      @aliases = {}
      Debugger.constants.grep(/.Command$/).each do |command|
        # Note: there is probably a non-eval way to instantiate the
        # command, but I don't know it. And eval works.
        cmd = Debugger.instance_eval("Debugger::#{command}.new")

        # Give the command access to other parts of the debugger
        cmd.core = @core
        cmd.proc = self

        # Add to list of commands and aliases.
        cmd_name = cmd.class.const_get(:NAME_ALIASES)[0]
        aliases= cmd.class.const_get(:NAME_ALIASES)[1..-1]
        @commands[cmd_name] = cmd
        aliases.each {|a| @aliases[a] = cmd_name}
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../lib/core'
  core = Debugger::Core.new()
  dbg = Debugger::CmdProcessor.new(core)
  dbg.msg('cmdproc main')
  dbg.errmsg('Whoa!')
  cmds = dbg.instance_variable_get('@commands')
  p cmds.keys
  p dbg.instance_variable_get('@aliases')
  cmd_name, cmd_obj = cmds.first
  puts cmd_obj.class.const_get(:HELP)
  puts cmd_obj.class.const_get(:SHORT_HELP)

  if ARGV.size > 0
    dbg.msg('Enter "q" to quit')
    dbg.process_commands
  else
    $input = []
    class << dbg
      def read_command
        $input.shift
      end
    end
    $input = ['1+2']
    dbg.process_command_and_quit?
  end
end
