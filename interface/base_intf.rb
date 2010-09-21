# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>

# A base class for a debugger interface.

class Trepan

  unless defined?(NotImplementedMessage)
    NotImplementedMessage = 'This method must be overriden in a subclass'
  end

  # A debugger interface handles the communication or interaction with between
  # the program and the outside portion which could be
  #  - a user, 
  #  - a front-end that talks to a user, or
  #  - another interface in another process or computer
  class Interface

    attr_accessor :interactive, :input, :output

    unless defined?(YES)
      YES = %w(y yes oui si yep ja)
      NO  = %w(n no non nope nein)
      YES_OR_NO = YES + NO
    end

    def initialize(inp=nil, out=nil, opts={})
      @input       = inp || STDIN
      @interactive = false 
      @opts        = opts
      @output      = out || STDOUT
    end

    # Closes all input and/or output.
    def close
      @input.close  unless @input.closed?
      @output.close unless @output.closed?
    end

    # Called when a dangerous action is about to be done to make sure
    # it's okay. `prompt' is printed; user response is returned.
    def confirm(prompt, default=false)
      raise RuntimeError, Trepan::NotImplementedMessage
    end

    # Common routine for reporting debugger error messages.
    def errmsg(str, prefix='** ')
      if str.is_a?(Array)
        str.each{|s| errmsg(s)}
      else
        str.split("\n").each do |s|
          msg("%s%s" % [prefix, s])
        end
      end
    end

    def finalize(last_wishes=nil)
      close
    end

    # Return true if interface is interactive.
    def interactive?
      # Default false and making subclasses figure out how to determine
      # interactiveness.
      false 
    end

    # used to write to a debugger that is connected to this
    # server; `str' written will have a newline added to it.
    def msg(message)
      if message.is_a?(Array)
        message.each{|s| msg(s)}
      else
        message = message ? message.to_s + "\n" : ''
        @output.write(message)
      end
    end

    # used to write to a debugger that is connected to this
    # server; `str' written will not have a newline added to it
    def msg_nocr(msg)
      @output.write(msg)
    end

    def read_command( prompt)
      raise RuntimeError, Trepan::NotImplementedMessage
    end

    def readline(prompt='')
      @output.flush
      @output.write(prompt) if prompt and prompt.size > 0
      @input.readline
    end
  end
end
