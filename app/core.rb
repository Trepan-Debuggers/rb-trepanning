# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2014-2015 Rocky Bernstein <rockyb@rubyforge.net>

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
        attr_accessor :top_skip     # Number of top frames to ignore
        attr_accessor :step_events  # bitmask of events - used only when
                                    # we are stepping
        attr_accessor :unmaskable_events
        attr_reader :trace_point

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
            @top_skip    = 0
            @trace_point = nil
        end

        def step_events_list
            puts "To be completed..."
        end

        # A trace-hook processor for tracepoints
        def event_processor_tp(tp)
            ## FIXME: tracepoint has an arg param. Figure out how to use it.
            @trace_point = tp
            retval = event_processor(tp.frame, tp.event)
            @trace_point = nil
            return retval
        end

        # A trace-hook processor with the interface a trace hook should have.
        def event_processor(frame, event, hook_arg=nil)

            return_exception = nil
            # FIXME: check for breakpoints or other unmaskable events.
            # For now there are none.

            return if @mutex.locked? and Thread.current == @current_thread

            @mutex.synchronize do
                @current_thread = Thread.current
                @frame = frame

                if dbgr.trace_filter.member?(@frame.method)
                    # puts "Not tracing #{@frame.method}"
                    return
                 end

                if @step_count > 0
                    @step_count -= 1
                    break
                elsif @step_count < 0 && ! @unmaskable_events.member?(event.to_s)
                    break
                end

                @event    = event
                @hook_arg = hook_arg

                ### debug:
                ### puts "#{frame.file[1]}:#{frame.source_location[0]}:in `#{frame.method}' #{event}"
                # if %w(line).member?(event)
                @processor.process_commands(@frame, top_skip)

                # Nil out variables just in case...

                return_exception = @exception
                @frame = @event = @arg = @exception = nil
                @top_skip = 0
            end
            return return_exception
        end

        # A trace-hook processor for 'trace var'
        def trace_var_processor(var_name, value)
            frame = RubyVM::Frame.get

            @step_count = 0  # Make event processor stop
            event_processor(frame,  'trace-var', [var_name, value])
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
        y = File.basename("foo.rb", ".rb")
        puts "yeah"
    end
end
