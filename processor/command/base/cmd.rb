# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
# Base class of all commands. Code common to all commands is here.
# Note: don't end classname with Command (capital C) since main
# will think this a command name like QuitCommand 
require 'columnize'

class Trepan
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
      width = settings[:maxwidth]
      Columnize::columnize(commands, width, ' ' * 4, 
                           true, true, ' ' * 2).chomp
    end

    def columnize_numbers(commands)
      width = settings[:maxwidth]
      Columnize::columnize(commands, width, ', ',
                           false, false, ' ' * 2).chomp
    end

    # FIXME: probably there is a way to do the delegation to proc methods
    # without having type it all out.

    def confirm(message, default)
      @proc.confirm(message, default)
    end

    def errmsg(message, opts={})
      @proc.errmsg(message, opts)
    end

    def obj_const(obj, name)
      obj.class.const_get(name) 
    end

    def msg(message, opts={})
      @proc.msg(message, opts)
    end

    # Convenience short-hand for @dbgr.intf[-1].msg_nocr
    def msg_nocr(msg)
      @proc.msg_nocr(msg, opts={})
    end

    def my_const(name)
      # Set class constant SHORT_HELP to be the first line of HELP
      # unless it has been defined in the class already.
      # The below was the simplest way I could find to do this since
      # we are the super class but want to set the subclass's constant.
      # defined? didn't seem to work here.
      c = self.class.constants
      if c.member?(:HELP) and !c.member?(:SHORT_HELP)
        short_help = self.class.const_get(:HELP).split("\n")[0].chomp('.')
        self.class.const_set(:SHORT_HELP, short_help)
      end
      self.class.const_get(name)
    end

    def name
      self.class.const_get(:NAME)
    end

    # The method that implements the debugger command.
    def run(*args)
      raise RuntimeError, 'You need to define this method elsewhere'
    end

    def section(message, opts={})
      @proc.section(message, opts)
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
