# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
# I/O related command processor methods
require_relative '../app/util'
class Trepan
  class CmdProcessor
    def errmsg(message)
      @dbgr.intf[-1].errmsg(safe_rep(message))
    end

    def msg(message)
      @dbgr.intf[-1].msg(safe_rep(message))
    end

    def msg_nocr(message)
      @dbgr.intf[-1].msg_nocr(safe_rep(message))
    end

    def read_command()
      @dbgr.intf[-1].read_command(@prompt)
    end

    def safe_rep(str)
      Trepan::Util::safe_repr(str, @settings[:maxstring])
    end
  end
end
