class Debugger
  class CmdProcessor
    def initialize()
      @event    = nil
      @frame    = nil
      @prompt   = '(rdbgr): '
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

    def msg(message)
      puts message
    end

    def process_command()
      str = read_command()
      return true if !str || 'q' == str.strip
      puts debug_eval(str)
      return false
    end

    def process_commands(frame=nil)
      @frame = frame
      leave_loop = false
        while not leave_loop do
          leave_loop = process_command()
        end
    rescue IOError, Errno::EPIPE
    rescue Exception
      puts "INTERNAL ERROR!!! #{$!}\n" rescue nil
    end

    def read_command()
      require 'readline'
      Readline.readline(@prompt)
    end

    def self.load_commands
      cmd_dir = File.join(File.dirname(__FILE__), 'command')
      # FIXME: File.directory?(cmd_dir)  ? 
      Dir.chdir(cmd_dir) do
        Dir.glob('*.rb').each do |rb|
          require_relative rb
        end
      end
    end
    load_commands
  end
end

if __FILE__ == $0
  dbg = Debugger::CmdProcessor.new()
  dbg.msg('cmdproc main')
  dbg.errmsg('Whoa!')
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
    dbg.process_command
  end
end
