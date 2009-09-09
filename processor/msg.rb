# I/O related command processor methods
class Debugger
  class CmdProcessor
    def errmsg(message)
      @dbgr.intf.errmsg(message)
    end

    def msg(message)
      @dbgr.intf.msg(message)
    end

    def read_command()
      @dbgr.intf.readline(@settings[:prompt])
    end
  end
end
