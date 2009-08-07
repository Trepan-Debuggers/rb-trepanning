# -*- coding: utf-8 -*-
# Base class of all commands. Code common to all commands is here.

class Debugger
  class Command
    attr_accessor :core, :proc

    def initialize(proc)
      @proc = proc
    end

    def errmsg(message)
      @proc.errmsg(message)
    end
    # FIXME: probably there is a way to create 
    # category, help, and short_help methods in a loop.
    def category
      my_const(:CATEGORY)
    end
    def help
      my_const(:HELP)
    end
    def msg(message)
      @proc.msg(message)
    end
    def run(args)
      raise RuntimeError, 'You need to define this method elsewhere'
    end

    def my_const(name)
      self.class.const_get(name)
    end

    def settings
      @proc.settings
    end

    def short_help
      my_const(:SHORT_HELP)
    end
  end
end
