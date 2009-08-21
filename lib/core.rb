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
    attr_reader   :dbgr         # Debugger instance
    attr_reader   :event        # String - event which triggering event
                                # processor
    attr_reader   :event_proc   # Proc of method event_processor
    attr_reader   :frame        # ThreadFrame instance
    attr_accessor :processor    # Debugger::CmdProc instance
    attr_reader   :settings     # Hash of things you can configure
    attr_accessor :step_count   # Fixnum. Negative means no tracing,
                                # 0 means stop on next event, 1 means 
                                # ignore one event. Step events gives the
                                # kind of things to count as a step.
    attr_accessor :step_events  # bitmask of events - used only when 
                                # we are stepping
    attr_accessor :async_events # bitmask of asyncronous events - used all
                                # the time

    include Trace

    unless defined?(DEFAULT_SETTINGS)
      # Synchronous events
      STEPPING_EVENT_MASK = 
        LINE_EVENT_MASK     | CLASS_EVENT_MASK    | CALL_EVENT_MASK  |
        RETURN_EVENT_MASK   | C_CALL_EVENT_MASK   | C_RETURN_EVENT_MASK

      ASYNC_EVENT_MASK = 
        RAISE_EVENT_MASK    | VM_EVENT_MASK       | SWITCH_EVENT_MASK

      DEFAULT_SETTINGS = {
        :cmdproc_opts => {},
        :step_count   => 0,  # Stop at next event
        :step_events  => DEFAULT_EVENT_MASK,
        :async_events => ASYNC_EVENT_MASK
      } 

    end

    def initialize(debugger, settings={})
      @dbgr         = debugger
      @event_proc   = self.method(:event_processor).to_proc
      @settings     = DEFAULT_SETTINGS.merge(settings)
      @step_count   = @settings[:step_count]
      @step_events  = @settings[:step_events]
      @async_events = @settings[:async_events]

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

      # FIXME: There should be a Trace.event_mask which should return the first
      # mask that matches the given trace hook.
      if @step_count < 0
        # If we are continuing, no need to stop at stepping events.
        Trace.event_masks[0] &= ~STEPPING_EVENT_MASK 
      else
        # Set to trace only those event we are interested in 
        # Don't step into calls of remaining portion
        step_count_save = step_count
        @step_count     = -1 
        dbgr.trace_filter.set_trace_func(@event_proc) unless
          RubyVM::TraceHook::trace_hooks.member?(@event_proc)
          
        # FIXME: this doesn't work. Bug in rb-trace? 
        # Trace.event_masks[0] = @step_events | @async_events
        RubyVM::TraceHook::trace_hooks[0].event_mask = @step_events | @async_events
        @step_count = step_count_save
      end

      # FIXME: unblock other threads

    end

    # A Ruby 1.8-style event processor. We don't use file, line, id, bind. 
    def old_event_processor(event, file, line, id, bind, klass)
      event_processor(event, RubyVM::ThreadFrame.current.prev)
    end

    # Call this from inside the program you want to get a synchronous
    # call to the debugger. set prev_count to the number of levels 
    # *before* the caller you want to skip.
    def debugger(prev_count=0)
      while @frame.type == 'IFUNC'
        @frame = @frame.prev
      end
      frame = RubyVM::ThreadFrame.current.prev(prev_count+1)
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
