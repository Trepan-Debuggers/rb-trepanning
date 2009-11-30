# -*- coding: utf-8 -*-
# Simulate I/O using lists of strings.

require_relative 'base_io'

# Simulate I/O using an array of strings. Sort of like StringIO, but
# even simplier.
class Debugger::StringArrayInput < Debugger::InputBase

  def initialize(inp, opts={})
    super
    @closed = false
  end

  # this close() interface is defined for class compatibility
  def close
    @closed = true 
  end

  def closed?
    @closed
  end

  def eof?
    @closed || @input.empty?
  end

  # Nothing to do here. Interface is for compatibility
  def flush ; end

  # Read a line of input. EOFError will be raised on EOF.  
  # Note that we don't support prompting
  def readline
    raise EOFError if eof?
    if @input.empty?
      raise EOFError
    end
    line = @input.shift
    return line 
  end

  class << self
    # Use this to set where to read from.
    def open(inp, opts={})
      if inp.is_a?(Array)
        return self.new(inp)
      else
        raise IOError, "Invalid input type (%s) for %s" % [inp.class, inp]
      end
    end
  end
end

# Simulate I/O using an array of strings. Sort of like StringIO, but
# even simplier.
class Debugger::StringArrayOutput < Debugger::OutputBase

  def initialize(out=[], opts={})
    super 
    @closed = false
  end

  # Nothing to do here. Interface is for compatibility
  def close
    @closed = true 
  end

  def closed?
    @closed
  end

  def eof?
    @closed || @output.empty? 
  end

  # Nothing to do here. Interface is for compatibility
  def flush ; end

  # This method the debugger uses to write. In contrast to
  # writeline, no newline is added to the end to `str'.
  #
  def write(msg)
    raise ValueError if @closed
    @output << msg
  end
  
  # used to write to a debugger that is connected to this
  # server; Here, we use the null string '' as an indicator of a
  # newline.
  def writeline(msg)
    write(msg)
    write('')
  end

  class << self
    # Use this to set where to write to. output can be a 
    # file object or a string. This code raises IOError on error.
    # 
    # If another file was previously open upon calling this open,
    # that will be stacked and will come back into use after
    # a close_write().
    def open(output=[])
      if output.is_a?(Array)
        return self.new(output)
      else
        raise IOError, ("Invalid output type (%s) for %s" % 
                        [output.class, output])
      end
    end
  end
end

# Demo
if __FILE__ == $0
  inp  = Debugger::StringArrayInput.open(['Now is the time', 'for all good men'])
  line = inp.readline
  p line
  line = inp.readline
  p line
  begin
    line = inp.readline
  rescue EOFError
    puts 'EOF hit on read'
  end

  out = Debugger::StringArrayOutput.open
  p out.output
#    line = io.readline('Type some more characters: ')
  out.writeline('Hello, world!')
  p out.output
  out.write('Hello');
  p out.output
  out.writeline(', again.');
  p out.output
#     io.open_write(sys.stdout)
  out.flush_after_write = true
  out.write('Last hello')
  puts "Output is closed? #{out.closed?}"
  out.close
  p out.output
  begin
    out.writeline("You won't see me")
  rescue
  end

  # Closing after already closed is okay
  out.close
  puts "Output is closed? #{out.closed?}"
  puts "Input is closed? #{inp.closed?}"
  inp.close
  puts "Input is closed? #{inp.closed?}"
end

