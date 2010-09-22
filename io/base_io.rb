# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
# classes to support communication to and from the debugger.  This
# communcation might be to/from another process or another computer.
# And reading may be from a debugger command script.
# 
# For example, we'd like to support Sockets, and serial lines and file
# reading, as well a readline-type input. Encryption and Authentication
# methods might decorate some of the communication channels.
# 
# Some ideas originiated as part of Matt Fleming's 2006 Google Summer of
# Code project.

class Trepan

  NotImplementedMessage = 'This method must be overriden in a subclass' unless
    defined?(NotImplementedMessage)
  
  class InputBase
    attr_reader   :input
    attr_reader   :line_edit

    DEFAULT_OPTS = {
      :line_edit => false,
    } unless defined?(DEFAULT_OPTS)

    def initialize(inp, opts={})
      @opts      = DEFAULT_OPTS.merge(opts)
      @input     = inp
      @line_edit = opts[:line_edit]
    end

    def close
      @input.close unless @input.closed?
    end

    def eof? 
      begin
        @input.eof?
      rescue IOError
        true
      end
    end

    # Read a line of input. EOFError will be raised on EOF.  
    #
    #   Note that we don't support prompting first. Instead, arrange
    #  to call Trepan::Output.write() first with the prompt. If
    # `use_raw' is set raw_input() will be used in that is supported
    #    by the specific input input. If this option is left None as is
    #    normally expected the value from the class initialization is
    #    used.
    def readline
      @input.readline
    end
  end

  # This is an abstract class that specifies debugger output.
  class OutputBase
    attr_accessor :flush_after_write
    attr_reader   :output
    def initialize(out, opts={})
      @output = out
      @flush_after_write = false
    end

    def close
      @output.close if @output
    end

    def eof? 
      @input.eof?
    end

    def flush
      @output.flush
    end

    # Use this to set where to write to. output can be a 
    # file object or a string. This code raises IOError on error.
    def write(*args)
      @output.print(*args)
    end

    # used to write to a debugger that is connected to this
    # `str' written will have a newline added to it
    #
    def writeline( msg)
      @output.write("%s\n" % msg)
    end
  end
end

