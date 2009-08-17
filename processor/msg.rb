# I/O related command processor methods
class Debugger
  # FIXME: All of this should go use an interface.
  class CmdProcessor
    def errmsg(message)
      puts "Error: #{message}"
    end

    def msg(message)
      puts message
    end

    def read_command()
      require 'readline'
      Readline.readline(@settings[:prompt])
    end
  end
end
