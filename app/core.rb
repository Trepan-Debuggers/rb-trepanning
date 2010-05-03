# -*- coding: utf-8 -*-
require 'thread_frame'
require 'trace'
# require_relative '../../rb-trace/app/trace'
require_relative '../processor/main'
class Debugger
  # This class contains the Debugger core routines, such as an event
  # processor which is responsible of handling what to do when an event is
  # triggered.
  # 
  # See also 'rdbgr' the top-level Debugger class and command-line routine
  # which ultimately will call this.

  class Core
    attr_accessor :async_events # bitmask of asyncronous events - used all
                                # the time
    attr_reader   :dbgr         # Debugger instance
    attr_reader   :event        # String - event which triggering event
                                # processor
    attr_reader   :event_proc   # Proc of method event_processor
    attr_accessor :exception    # Return exception to pass back. A 'raise'
                                # command can set this.
    attr_reader   :frame        # ThreadFrame instance
    attr_reader   :hook_arg     # 'arg' passed from trace hook
    attr_accessor :mutex        # mutex to lock out other threads from
                                # entering debugger while we are in it.
    attr_accessor :processor    # Debugger::CmdProc instance
    attr_reader   :settings     # Hash of things you can configure
    attr_accessor :step_count   # Fixnum. Negative means no tracing,
                                # 0 means stop on next event, 1 means 
                                # ignore one event. Step events gives the
                                # kind of things to count as a step.
    attr_accessor :step_events  # bitmask of events - used only when 
                                # we are stepping
    attr_accessor :unmaskable_events 

    include Trace

    unless defined?(CORE_DEFAULT_SETTINGS)
      # Synchronous events
      STEPPING_EVENT_MASK = 
        LINE_EVENT_MASK     | CLASS_EVENT_MASK    | CALL_EVENT_MASK     |
        RETURN_EVENT_MASK   | C_CALL_EVENT_MASK   | C_RETURN_EVENT_MASK |
        INSN_EVENT_MASK     | BRKPT_EVENT_MASK

      ASYNC_EVENT_MASK = 
        RAISE_EVENT_MASK    | VM_EVENT_MASK       | SWITCH_EVENT_MASK

      CORE_DEFAULT_SETTINGS = {
        :cmdproc_opts      => {},
        :debug_core_events => false,
        :hook_name         => :event_processor, # or :old_event_processor
        :step_count        => 0,                # Stop at next event
        :async_events      => ASYNC_EVENT_MASK,

        # FIXME:
        # Somewhere inside the VM we allow I guess nested tracing which is messing
        # up ThreadFrame pointers and information. When this is fixed we can do the below.
        # Until then we need to at least remove C calls and returns and possibly other
        # events as well.
        # :step_events       =>  
	# (DEFAULT_EVENT_MASK | INSN_EVENT_MASK)

        :step_events       =>  
	(DEFAULT_EVENT_MASK | INSN_EVENT_MASK) &
	~(C_CALL_EVENT_MASK | C_RETURN_EVENT_MASK)
      } 

    end

    def initialize(debugger, settings={})
      @dbgr         = debugger
      @exception    = nil
      @mutex        = Mutex.new
      @settings     = CORE_DEFAULT_SETTINGS.merge(settings)

      @step_count   = @settings[:step_count]
      @step_events  = @settings[:step_events]
      @async_events = @settings[:async_events]
      @debug_events = @settings[:debug_core_events]

      hook_name     = @settings[:hook_name]
      @event_proc   = self.method(hook_name).to_proc
      @processor    = CmdProcessor.new(self, @settings[:cmdproc_opts])
      @unmaskable_events = %w(brkpt raise switch vm)
    end

    # A trace-hook processor with the interface a trace hook should have.
    def event_processor(event, frame, arg=nil)

      return_exception = nil
      # FIXME: check for breakpoints or other unmaskable events. 
      # For now there are none.
      
      @mutex.synchronize do
        @frame = frame
        while @frame.type == 'IFUNC'
          @frame = @frame.prev
        end
        
        if @step_count > 0
          @step_count -= 1
          break
        elsif @step_count < 0 && ! @unmaskable_events.member?(event)
          break
        end

        @event    = event
        @frame    = frame
        @hook_arg = arg
        
        if @settings[:debug_core_events]
          msg "event #{event} #{@frame.source_container.inspect} #{@frame.source_location.inspect}"
        end
        @processor.process_commands(@frame)
        
        # FIXME: There should be a Trace.event_mask which should return the first
        # mask that matches the given trace hook.
        if @step_count < 0
          # If we are continuing, no need to stop at stepping events.
          Trace.event_masks[0] &= ~STEPPING_EVENT_MASK 
        else
          # Set to trace only those events we are interested in.  
          
          # Don't step/trace into Ruby routines called from here in the code
          # below (e.g. "trace_hooks").
          step_count_save = step_count
          @step_count     = -1 
          
          unless @event_proc == dbgr.trace_filter.hook_proc
            dbgr.trace_filter.add_trace_func(@event_proc) 
          end
          
          # FIXME: this doesn't work. Bug in rb-trace? 
          # Trace.event_masks[0] = @step_events | @async_events
          RubyVM::TraceHook::trace_hooks[0].event_mask = 
            @step_events | @async_events
          @step_count = step_count_save
        end
        
        # Nil out variables just in case...
        
        return_exception = @exception
        @frame = @event = @arg = @exception = nil

      end
      return return_exception 
    end

    # A Ruby 1.8-style event processor. We don't use file, line, id, bind. 
    def old_event_processor(event, file, line, id, bind, klass)
      event_processor(event, RubyVM::ThreadFrame.current.prev)
    end

    # Call this from inside the program you want to get a synchronous
    # call to the debugger. set prev_count to the number of levels 
    # *before* the caller you want to skip.
    def debugger(prev_count=0)
      while @frame && @frame.type == 'IFUNC'
        @frame = @frame.prev
      end
      frame = RubyVM::ThreadFrame.current.prev(prev_count+1)
      @step_count = 0  # Make event processor stop
      event_processor('debugger-call', frame)
    end
    
    # A trace-hook processor for 'trace var'
    def trace_var_processor(val)
      frame = RubyVM::ThreadFrame.current.prev
      if 'CFUNC' == frame.type
        # Don't need the C call that got us here.
        prev = frame.prev
        frame = frame.prev if prev
      end

      # Stop future tracing into the debugger
      Thread.current.tracing = true  
      
      @step_count = 0  # Make event processor stop
      event_processor('trace-var', frame)
    end

  end
end
if __FILE__ == $0
  require_relative '../lib/rbdbgr'
  dbg = Debugger.new()
  if ARGV.size > 0
    def foo(dbg)
      p 'foo here'
      dbg.debugger(:immediate=>true)
    end
    foo(dbg)
  end
end
