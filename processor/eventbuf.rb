# For recording hook events in a buffer for review later. Make use of
# Trace::Buffer for this prupose.
require 'trace'

class Debugger
  class CmdProcessor

    attr_reader :eventbuf
    attr_reader :event_tracefilter

    def eventbuf_initialize(size=100)
      @eventbuf = TraceBuffer.new(size)
      # @event_tracefilter = TraceFilter.new
    end
    
    # def event_processor(event, frame, arg=nil)
    #   @eventbuf.append(event, frame, arg)
    # end

    # FIXME temporary routine
    def dump_all(from=nil, to=nil)
      @eventbuf.each(from, to) do |e| 
        puts @eventbuf.format_entry(e) if e
      end
    end

    # FIXME: multiple hook mechanism needs work. 
    # def start_capture
    #   @event_tracefilter.set_trace_func(method(:event_processor).to_proc,
    #                                     Trace::DEFAULT_EVENT_MASK)
    # end

    # def stop_capture
    #   @event_tracefilter.set_trace_func(nil)
    # end

  end
end
if __FILE__ == $0
  # Demo it.
  cmdproc = Debugger::CmdProcessor.new
  cmdproc.eventbuf_initialize(5)
  # cmdproc.start_capture
  # z=5
  # z.times do |i|
  #   x = i
  #   y = x+2
  # end
  # cmdproc.stop_capture
  cmdproc.dump_all
end
