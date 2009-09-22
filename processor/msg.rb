# I/O related command processor methods
class Debugger
  class CmdProcessor
    def errmsg(message)
      @dbgr.intf[-1].errmsg(message)
    end

    def msg(message)
      @dbgr.intf[-1].msg(message)
    end

    def read_command()
      @dbgr.intf[-1].read_command(@settings[:prompt])
    end
  end
end
