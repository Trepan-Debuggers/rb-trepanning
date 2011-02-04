# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'trace'
require 'columnize'
require_relative '../base/subcmd'

class Trepan::Subcommand::SetEvents < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = "set events {event-name[,] ...}

Set trace events that the debugger will stop on

Valid event names come from the Trace module and include:
#{Columnize::columnize(Trace.const_get('EVENTS'), 80, ' ' * 4, true, true, ' ' * 2)}

Separate event names with space and an optional comma is also
allowable after an event name.

Examples:
   set events call return
   set ev call, c_call, return, c_return, c_return, insn
"
    MIN_ABBREV   = 'ev'.size
    SHORT_HELP   = 'Set trace events we may stop on.'
  end

  completion Trace.const_get("EVENTS").map{|event| event.to_s}

  def save_command
    step_events_list = @proc.core.step_events_list
    step_events_list = 'brkpt' unless step_events_list
    ["#{subcmd_prefix_string} #{step_events_list}"]
  end

  def run(args)
    unless args.size <= 2
      events = args[2..-1]
      events.each {|event| event.chomp!(',')}
      bitmask, bad_events = Trace.events2bitmask(events)
      bitmask |= Trace::BRKPT_EVENT_MASK
      unless bad_events.empty?
        errmsg("Event names unrecognized/ignored: %s" % bad_events.join(', '))
      end
      @proc.core.step_events = bitmask
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
  subcommand.summary_help(name)
  puts
  subcommand.run([])
  [%w(call line foo), %w(insn, c_call, c_return,)].each do |events|
    subcommand.run(%w(set events) + events)
    puts 'bitmask: %09b, events: %s ' % [dbgr.core.step_events, events.inspect]
  end
  puts '-' * 20
  puts subcommand.save_command()

end
