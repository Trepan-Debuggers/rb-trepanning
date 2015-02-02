# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require 'columnize'
require_relative '../base/subcmd'


class Trepan::Subcommand::SetEvents < Trepan::Subcommand


    unless defined?(HELP)
        TRACE_POINT_EVENTS = TracePoint.new{}.event_mask.map{|e| e.to_s}
        Trepanning::Subcommand.set_name_prefix(__FILE__, self)
        HELP         = <<-EOH
**#{PREFIX.join(' ')}** {*event-name*[,] ...}

Set trace events that the debugger will stop on.

Event names are:

#{Columnize::columnize(TRACE_POINT_EVENTS, 80, ' ' * 4,
                       true, true, ' ' * 4)}

Separate event names with space and an optional comma is also
allowable after an event name.

Examples:
---------
    set events call return
    set ev call, c_call, return, c_return, c_return

See also:
---------

`show events`

EOH
    MIN_ABBREV   = 'ev'.size
    SHORT_HELP   = 'Set trace events we may stop on.'
  end

  completion TRACE_POINT_EVENTS

    def save_command
        step_events_list = @proc.core.step_events_list
        step_events_list = 'brkpt' unless step_events_list
        ["#{subcmd_prefix_string} #{step_events_list}"]
    end

    def run(args)
        unless args.size <= 2
            events = args[2..-1]
            invalid_names = []
            valid_events = events.map do |event|
                event.chomp!(',')
                if TRACE_POINT_EVENTS.member?(event)
                    event
                else
                    invalid_names << event
                    nil
                end
            end.compact

            # FIXME: check validity of events
            if @proc.core.trace_point
                @proc.core.trace_point.event_mask_set *valid_events
            else
                errmsg("No tracepoint currently set")
            end
            unless invalid_names.empty?
                errmsg("Event names unrecognized/ignored: %s" % invalid_names.join(', '))
            end
        end
        @proc.commands['show'].subcmds.subcmds[:events].run('events')
    end
end

if __FILE__ == $0
    # Demo it.
    require_relative '../../mock'
    name = File.basename(__FILE__, '.rb')

    # FIXME: DRY the below code
    dbgr, cmd = MockDebugger::setup('set')
    subcommand = Trepan::Subcommand::SetEvents.new(cmd)
    testcmdMgr = Trepan::Subcmd.new(subcommand)

    name = File.basename(__FILE__, '.rb')
    # subcommand.summary_help(name)
    puts
    subcommand.run([])
    [%w(call line foo), %w(c_call, c_return,)].each do |events|
        subcommand.run(%w(set events) + events)
        puts dbgr.core.step_events
    end
    puts '-' * 20
    puts subcommand.save_command()

end
