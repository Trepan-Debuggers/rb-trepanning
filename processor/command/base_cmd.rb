# Base class of all commands. Code common to all commans is here.

class Debugger
  class Command
    attr_accessor :core, :proc
    def errmsg(message)
      @proc.errmsg(message)
    end
    # FIXME: probably there is a way to create 
    # category, help, and short_help methods in a loop.
    def category
      self.class.const_get(:CATEGORY)
    end
    def help
      self.class.const_get(:HELP)
    end
    def msg(message)
      @proc.msg(message)
    end
    def run(args)
      raise RuntimeError, 'You need to define this method elsewhere'
    end

    def settings
      @proc.settings
    end

    def short_help
      self.class.const_get(:SHORT_HELP)
    end
  end
end
