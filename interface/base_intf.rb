# -*- coding: utf-8 -*-
# A base class for a debugger interface.

## FIXME: hook into to-be-written input/output subsystem.

class Debugger

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
      @eof         = false # FIXME remove me
    end

    # Closes all input and/or output.
    def close
      @input.close
      @output.close
    end

    # Called when a dangerous action is about to be done to make sure
    # it's okay. `prompt' is printed; user response is returned.
    def confirm(prompt, default=false)
      raise RuntimeError, Debugger::NotImplementedMessage
    end

    # FIXME remove me
    def eof?
      @eof
    end

    # Common routine for reporting debugger error messages.
    def errmsg(str, prefix='*** ')
      raise RuntimeError, Debugger::NotImplementedMessage
    end

    def finalize(last_wishes=nil)
      close
    end

    # used to write to a debugger that is connected to this
    # server; `str' written will have a newline added to it.
    def msg(msg)
      @output.write(msg)
    end

    # used to write to a debugger that is connected to this
    # server; `str' written will not have a newline added to it
    def msg_nocr(msg)
      # FIXME: use method from input.
      @output.print(msg)
    end

    def read_command( prompt)
      raise RuntimeError, Debugger::NotImplementedMessage
    end

    def readline(prompt, add_to_history=true)
      raise RuntimeError, Debugger::NotImplementedMessage
    end
  end
end
