class Debugger
  class EventBuffer
    EventStruct = Struct.new(:event, :arg, :type, :thread, :method,
                             :source_container, :source_location,
                             :iseq, :pc_offset) unless defined?(EventStruct)
    attr_reader   :buf
    attr_reader   :size   # Max size of buffer or nil if unlimited.
    attr_accessor :marks  # User position mark into buffer. If buffer is limited,
                          # then marks will drop out as they disappear from the buffer
    def initialize(size=nil)
      @size = size
      reset
    end

    def reset
      @buf   = []
      @marks = []
      @pos   = -1
    end

    # Add a new event dropping off old events if that was declared
    # marks are also dropped if buffer has a limit.
    def append(event, frame, arg)
      iseq = frame.iseq
      item = EventStruct.new(event, arg, frame.type, frame.thread, frame.method,
                             frame.source_container, frame.source_location,
                             iseq, iseq ? frame.pc_offset : nil)
      @pos += 1 
      @pos = 0 if @size && @pos == @size
      @marks.shift if @marks[0] == @pos
      @buf[@pos] = item
    end

    def add_mark
      @marks << @pos
    end
  end
end
if __FILE__ == $0
  def event_processor(event, frame, arg=nil)
    begin 
      @eventbuf.append(event, frame, arg)
    rescue
      p $!
    end
  end
  @eventbuf = Debugger::EventBuffer.new(5)
  require 'trace'
  trace_filter = TraceFilter.new
  trace_func   = method(:event_processor).to_proc
  trace_filter << trace_func
  trace_filter.set_trace_func(trace_func)
  x = 1
  @eventbuf.buf.each { |e| p e}
end
