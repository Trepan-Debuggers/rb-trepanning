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
      @dbgr.intf.pop if @dbgr.intf[-1].eof? && @dbgr.intf.size > 1
      return false if @dbgr.intf[-1].eof? 
      @dbgr.intf[-1].read_command(@settings[:prompt])
    end
  end
end
