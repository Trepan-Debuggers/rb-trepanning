# I/O related command processor methods
class Debugger
  class CmdProcessor
    def errmsg(message)
      @dbgr.intf[-1].errmsg(saferep(message))
    end

    def msg(message)
      @dbgr.intf[-1].msg(saferep(message))
    end

    def msg_nocr(message)
      @dbgr.intf[-1].msg_nocr(saferep(message))
    end

    def read_command()
      @dbgr.intf[-1].read_command(@settings[:prompt])
    end
    private
    def saferep(str)
      if str.is_a?(String) && str.size > @settings[:maxstring] &&
          !str.index("\n")
        str[0...@settings[:maxstring]] + '...' 
      else
        str
      end
    end
  end
end
