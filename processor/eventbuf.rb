# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
# For recording hook events in a buffer for review later. Make use of
# Trace::Buffer for this prupose.
require 'trace'
require 'linecache'
require_relative 'virtual'

class Trepan
  class CmdProcessor < VirtualCmdProcessor

    attr_reader :eventbuf
    attr_reader :event_tracefilter

    def eventbuf_initialize(size=100)
      @eventbuf = Trace::EventBuffer.new(size)
      # @event_tracefilter = Trace::Filter.new
    end
    
    # def event_processor(event, frame, arg=nil)
    #   @eventbuf.append(event, frame, arg)
    # end

    # Print event buffer entries from FROM up to TO try to stay within
    # WIDTH. We show source lines only the first time they are
    # encountered. Also we use separators to indicate points that the
    # debugger has stopped at.
    def eventbuf_print(from=nil, to=nil, width=80)
      sep = '-' * ((width - 7) / 2)
      last_container, last_location = nil, nil
      if from == nil || !@eventbuf.marks[-1] 
        mark_index = 0
      else
        mark_index = @eventbuf.marks.size-1
        translated_from = @eventbuf.zero_pos + from
        @eventbuf.marks.each_with_index do
          |m, i|
          if m > translated_from
            mark_index = [0, i-1].max
            break
          elsif m == translated_from
            mark_index = i
            break
          end
        end
      end

      nextmark = @eventbuf.marks[mark_index]
      @eventbuf.each_with_index(from, to) do |e, i| 
        if nextmark 
          if nextmark == i
            msg "#{sep} %5d #{sep}" % (mark_index - @eventbuf.marks.size)
            mark_index += 1 if mark_index < @eventbuf.marks.size - 1
            nextmark = @eventbuf.marks[mark_index]
          elsif nextmark < i
            mark_index += 1 if mark_index < @eventbuf.marks.size - 1
            nextmark = @eventbuf.marks[mark_index]
          end
        end
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
      mess = "#{item.event} "
      mess +=
        if %w(c-return return).member?(item.event)
          "#{item.method} => #{item.arg.inspect}" + (same_loc ? '' : "\n\t")
        elsif 'c-call' == item.event
          "#{item.method} "
        else
          "#{item.type} #{item.method} "
        end
      mess += "#{container} at line #{location}" unless same_loc

      if item.iseq # && long_format
        mess += "\n\tVM offset #{item.pc_offset}"
      end
      unless same_loc
        text = LineCache::getline(container, location, 
                                  :reload_on_change => @reload_on_change)
        mess += ":\n  #{text.chomp}" if text
      end
      return container, location, mess
    end
      
    # FIXME: multiple hook mechanism needs work. 
    # def start_capture
    #   @event_tracefilter.add_trace_func(method(:event_processor).to_proc,
    #                                     Trace::DEFAULT_EVENT_MASK)
    # end

    # def stop_capture
    #   @event_tracefilter.set_trace_func(nil)
    # end

  end
end
if __FILE__ == $0
  # Demo it.
  cmdproc = Trepan::CmdProcessor.new([])
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
  cmdproc.eventbuf_print
end
