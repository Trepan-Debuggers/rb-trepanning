# -*- coding: utf-8 -*-
# Base class of all commands. Code common to all commands is here.
# Note: don't end classname with Command (capital C) since main
# will think this a command name like QuitCommand 

class Debugger
  class Command
    attr_accessor :core, :proc

    unless defined?(MIN_ARGS)
      MIN_ARGS      = 0      # run()'s args array must be at least this many
      MAX_ARGS      = nil    # run()'s args array must be at least this many
      NEED_STACK    = false  # We'll say that commands which need a stack
                             # to run have to declare that and those that
                             # don't don't have to mention it.
    end

    def initialize(proc)
      @name = my_const(:NAME)
      @proc = proc
    end

    def category
      my_const(:CATEGORY)
    end

    # List commands arranged in an aligned columns
    def columnize_commands(commands)
      width = settings[:width]
      Columnize::columnize(commands, width, ' ' * 4, 
                           true, true, ' ' * 2).chomp
    end

    def columnize_numbers(commands)
      width = settings[:width]
      Columnize::columnize(commands, width, ', ',
                           true, false, ' ' * 2).chomp
    end

    # FIXME: probably there is a way to do the delegation to proc methods
    # without having type it all out.

    def confirm(message, default)
      @proc.confirm(message, default)
    end

    def errmsg(message)
      @proc.errmsg(message)
    end

    def obj_const(obj, name)
      obj.class.const_get(name) 
    end

    def msg(message)
      @proc.msg(message)
    end

    # Convenience short-hand for @dbgr.intf[-1].msg_nocr
    def msg_nocr(msg)
      @proc.msg_nocr(msg)
    end

    def my_const(name)
      self.class.const_get(name)
    end

    # The method that implements the debugger command.
    def run(*args)
      raise RuntimeError, 'You need to define this method elsewhere'
    end

    def settings
      @proc.settings
    end

    def short_help
      help_constant_sym = if self.class.constants.member?(:SHORT_HELP) 
                            :SHORT_HELP 
                          else :HELP
                          end
      my_const(help_constant_sym)
    end
  end
end
