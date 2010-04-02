# For recording hook events in a buffer for review later. Make use of
# Trace::Buffer for this prupose.
require 'trace'
require 'linecache'

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
      last_container, last_location = nil, nil
      @eventbuf.each(from, to) do |e| 
        last_container, last_location, mess = 
          format_eventbuf_entry(e, last_container, last_location) if e
        msg mess
      end
    end

    # Show event buffer entry. If the location is the same as the previous
    # location we don't show the duplicated location information.
    def format_eventbuf_entry(item, last_container, last_location)
      container = 
        if item.source_container[0] == 'file'
          item.source_container[1]
        else
          item.source_container
        end
    
      location = 
        if 1 == item.source_location.size 
          item.source_location[0]
        else
          item.source_location
        end

      same_loc = (container == last_container && location == last_location)
      mess = "#{item.event} #{item.type} #{item.method} "
      mess += "#{container} at #{location}:" unless same_loc

      if item.iseq # && long_format
        mess += "\n\tVM offset #{item.pc_offset} of #{item.iseq.name}"
      end
      unless same_loc
        text = LineCache::getline(container, location, @reload_on_change).chomp
        mess += "\n  #{text}" if text
      end
      return container, location, mess
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

  def cmdproc.msg(mess)
    puts mess
  end
  # cmdproc.start_capture
  # z=5
  # z.times do |i|
  #   x = i
  #   y = x+2
  # end
  # cmdproc.stop_capture
  cmdproc.dump_all
end
