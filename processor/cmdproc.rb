# The main "driver" class for a command processor. Other parts of the 
# command class and debugger command objects are pulled in from here.

require_relative 'msg'
require_relative 'frame'
require_relative 'validate'

class Debugger
  class CmdProcessor
    attr_reader   :aliases      # Hash of command names indexed by alias name
    attr_reader   :commands     # Hash of command objects indexed by name
    attr_reader   :current_thread
    attr_accessor :frame        # ThreadFrame, current frame
    attr_accessor :frame_index  # frame index in a "where" command
    attr_accessor :top_frame    # top frame of current thread. Since
                                # right now the ThreadFrame method has "prev" 
                                # but no way to move in the other direction.
                                # So we store the top frame. 
    attr_reader   :threads2frames

    def initialize(core)
      @core           = core
      @current_thread = nil
      @event          = nil
      @frame          = nil
      @frame_index    = nil
      @prompt         = '(rdbgr): '
      @thread2frames  = {}
      @top_frame      = nil

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
      msg debug_eval(str)
      return false
    end

    # This is the main entry point.
    def process_commands(frame=nil)

      @frame_index    = 0
      @current_thread = Thread.current
      @frame = @top_frame = frame

      # Cache of top_frames we've encountered
      @threads2frames = {}
      @threads2frames[@current_thread] = @top_frame

      leave_loop = false
      while not leave_loop do
          leave_loop = process_command_and_quit?()
          # Might have other stuff here.
        end
    rescue IOError, Errno::EPIPE
    rescue Exception
      puts "INTERNAL ERROR!!! #{$!}\n" rescue nil

      # Remove access to @frame. 
      @top_frame = @frame = @frame_index = @current_thread = nil 
      @threads2frames = {}

    end

    # Loads in debugger commands by require'ing each ruby file in the
    # 'command' directory. Then a new instance of each class of the 
    # form Debugger::xxCommand is added to @commands and that array
    # is returned.
    def load_debugger_commands(cmd_dir)
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
