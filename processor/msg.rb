# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
# I/O related command processor methods
require_relative '../app/util'
class Trepan
  class CmdProcessor
    def errmsg(message, opts={})
      message = safe_rep(message) unless opts[:unlimited]
      if @settings[:highlight] && defined?(Term::ANSIColor)
        message = 
          Term::ANSIColor.italic + message + Term::ANSIColor.reset 
      end
      @dbgr.intf[-1].errmsg(message)
    end

    def msg(message, opts={})
      message = safe_rep(message) unless opts[:unlimited]
      @dbgr.intf[-1].msg(message)
    end

    def msg_nocr(message, opts={})
      message = safe_rep(message) unless opts[:unlimited]
      @dbgr.intf[-1].msg_nocr(message)
    end

    def read_command()
      @dbgr.intf[-1].read_command(@prompt)
    end

    def safe_rep(str)
      Util::safe_repr(str, @settings[:maxstring])
    end

    def section(message, opts={})
      message = safe_rep(message) unless opts[:unlimited]
      if @settings[:highlight] && defined?(Term::ANSIColor)
        message = 
          Term::ANSIColor.bold + message + Term::ANSIColor.reset 
      end
      @dbgr.intf[-1].msg(message)
    end
  end
end
