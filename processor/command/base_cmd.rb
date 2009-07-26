# Base class of all commands. Code common to all commans is here.

class Debugger
  class Command
    attr_accessor :core, :proc
    def errmsg(message)
      @proc.errmsg(message)
    end
    def msg(message)
      @proc.msg(message)
    end
    def run(args)
      raise RuntimeError, 'You need to define this method elsehwere'
    end
  end
end
