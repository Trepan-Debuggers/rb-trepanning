require 'thread_frame'
require 'trace'
require_relative %w(.. processor main)
class Debugger
  # This class contains the Debugger core routines, such as an event
  # processor is responsible of handling what to do when an event is
  # triggered.
  # 
  # See also 'rdbgr' the top-level Debugger class and command-line routine
  # which ultimately will call this.

  class Core
    attr_reader   :dbgr        # Debugger instance
    attr_reader   :event       # String - event which triggering event
                               # processor
    attr_reader   :frame       # ThreadFrame instance
    attr_accessor :processor   # Debugger::CmdProc instance
    attr_reader   :settings    # Hash of things you can configure
    attr_accessor :step_count  # Fixnum. Negative means no tracing,
                               # 0 means stop on next event, 1 means 
                               # ignore one event. Step events gives the
                               # kind of things to count as a step.
    attr_accessor :step_events # bitmask of events - used only when 
                               # we are stepping

    include Trace

    unless defined?(DEFAULT_SETTINGS)
      DEFAULT_SETTINGS = {
        :cmdproc_opts => {},
        :step_count   => 0,  # Stop at next event
        :step_events  => DEFAULT_EVENT_MASK
      } 

      # Synchronous events
      STEPPING_EVENT_MASK = 
        LINE_EVENT_MASK   | CLASS_EVENT_MASK    | 
        CALL_EVENT_MASK   | RETURN_EVENT_MASK   |
        C_CALL_EVENT_MASK | C_RETURN_EVENT_MASK

    end

    def initialize(debugger, settings={})
      @dbgr        = debugger
      @settings    = DEFAULT_SETTINGS.merge(settings)
      @step_count  = @settings[:step_count]
      @step_events = @settings[:step_events]
      @processor   = CmdProcessor.new(self, @settings[:cmdproc_opts])
    end

    # A trace-hook processor with the interface a trace hook should have.
    def event_processor(event, frame, arg=nil)

      # FIXME: Block all other threads
      # FIXME: check for breakpoints. For now there are none.
      if @step_count > 0
        @step_count -= 1
        return
      elsif @step_count < 0
        return
      end
        
      @arg   = arg
      @event = event
      @frame = frame
      while @frame.type == 'IFUNC'
        @frame = @frame.prev
      end

      @processor.process_commands(@frame)

      # FIXME: There should be a Trace.event_mask should return the first
      # mask that matches the given trace hook.
      if step_count < 0
        # If we are continuing, no need to stop at stepping events.
        Trace.event_masks[0] &= ~STEPPING_EVENT_MASK 
      else
        # Set to trace only those event we are interested in 
        Trace.event_masks[0] |= @step_events
      end

      # FIXME: unblock other threads

    end

    # A Ruby 1.8-style event processor. We don't use file, line, id, bind. 
    def old_event_processor(event, file, line, id, bind, klass)
      event_processor(event, RubyVM::ThreadFrame.current.prev)
    end

    # Call this from inside the program you want to get a synchronous
    # call to the debugger.
    def debugger
      frame = RubyVM::ThreadFrame.current.prev
      @step_count = 0
      event_processor('debugger-call', frame)
    end
    
  end
end
if __FILE__ == $0
  require_relative %w(.. rbdbgr)
  dbg = Debugger.new()
  if ARGV.size > 0
    def foo(dbg)
      p 'foo here'
      dbg.debugger(:immediate=>true)
    end
    foo(dbg)
  end
end
