# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2014 Rocky Bernstein <rockyb@rubyforge.net>

require 'set'
require_relative '../processor'

class Trepan
    # This class contains the Trepan core routines, such as an event
    # processor which is responsible of handling what to do when an event is
    # triggered.
    #
    # See also lib/trepanning.rb, the top-level Trepan class, and command-line routine
    # bin/trepan which ultimately will call this.

    class Core
        attr_accessor :async_events # bitmask of asyncronous events - used all
                                    # the time
        attr_reader   :dbgr         # Trepan instance
        attr_reader   :event        # String - event which triggering event
                                    # processor
        attr_reader   :event_proc   # Proc of method event_processor
        attr_accessor :exception    # Return exception to pass back. A 'raise'
                                    # command can set this.
        attr_reader   :frame        # ThreadFrame instance
        attr_reader   :hook_arg     # 'arg' passed from trace hook
        attr_accessor :mutex        # mutex to lock out other threads from
                                    # entering debugger while we are in it.
        attr_accessor :processor    # Trepan::CmdProc instance
        attr_reader   :settings     # Hash of things you can configure
        attr_accessor :step_count   # Fixnum. Negative means no tracing,
                                    # 0 means stop on next event, 1 means
                                    # ignore one event. Step events gives the
                                    # kind of things to count as a step.
        attr_accessor :step_events  # bitmask of events - used only when
                                    # we are stepping
        attr_accessor :unmaskable_events

        unless defined?(CORE_DEFAULT_SETTINGS)
            # Synchronous events
            STEPPING_EVENTS = Set.new([:line, :class, :call, :return, :c_call,
                                       :c_return, :raise, :b_call, :b_return,
                                       :thread_begin, :thread_end, :brkpt])
            ASYNC_EVENTS = Set.new([:raise])

            CORE_DEFAULT_SETTINGS = {
                :cmdproc_opts      => {},
                :debug_core_events => false,
                :hook_name         => :event_processor,
                :step_count        => 0,                # Stop at next event
                :async_events      => ASYNC_EVENTS,

                # Not sure what the "right" set really is. The below is just
                # a guess. Use "set events" or customize in ~/.trepanrc
                :step_events       => STEPPING_EVENTS - [:c_call, :c_return]
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
            @current_thread = nil
        end

        def step_events_list
            puts "To be completed..."
        end

        # An old-stype set_trace_func event processor. We don't use
        # file, line, id, bind.
        def old_event_processor(event, file, line, id, bind, klass)
            event_processor(event, RubyVM::Frame.get)
        end

        # A trace-hook processor with the interface a trace hook should have.
        def event_processor(event, frame, arg=nil)

            return_exception = nil
            # FIXME: check for breakpoints or other unmaskable events.
            # For now there are none.

            return if @mutex.locked? and Thread.current == @current_thread

            @mutex.synchronize do
                @current_thread = Thread.current
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
                @hook_arg = arg

                ### debug:
                ### puts "#{frame.file[1]}:#{frame.source_location[0]}:in `#{frame.method}' #{event}" # if %w(line).member?(event)
                @processor.process_commands(@frame)

                # Nil out variables just in case...

                return_exception = @exception
                @frame = @event = @arg = @exception = nil

            end
            return return_exception
        end

        # Call this from inside the program you want to get a synchronous
        # call to the debugger. set prev_count to the number of levels
        # *before* the caller you want to skip.
        def debugger(prev_count=0)
            while @frame && @frame.type == 'IFUNC'
                @frame = @frame.prev
            end
            frame = RubyVM::Frame.get(prev_count)
            @step_count = 0  # Make event processor stop
            event_processor('debugger-call', frame)
            set_trace_func(old_event_processor)
        end

        # A trace-hook processor for 'trace var'
        def trace_var_processor(var_name, value)
            frame = RubyVM::Frame.get(2)
            if 'CFUNC' == frame.type
                # Don't need the C call that got us here.
                prev = frame.prev
                frame = frame.prev if prev
            end

            @step_count = 0  # Make event processor stop
            event_processor('trace-var', frame, [var_name, value])
        end

    end
end
if __FILE__ == $0
    require_relative '../lib/trepanning'
    dbg = Trepan.new()
    if ARGV.size > 0
        def foo(dbg)
            p 'foo here'
            dbg.debugger(:immediate=>true)
        end
        foo(dbg)
        x = 5
    end
end
