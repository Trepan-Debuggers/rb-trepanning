# -*- coding: utf-8 -*-
require 'trace'
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::SetTraceset < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Set trace events we may stop on'
    MIN_ABBREV   = 'traces'.size
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = HELP
  end

  # FIXME: this really should be a subcommand of "set trace"
  def run(events)
    bitmask, bad_events = Trace.events2bitmask(events)
    unless bad_events.empty?
      errmsg("Event names unrecognized/ignored: %s" % bad_events.join(', '))
    end
    @proc.core.step_events = bitmask
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::SetTraceset.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  puts
  subcommand.run([])
  subcommand.run(['call', 'line', 'foo'])
end
