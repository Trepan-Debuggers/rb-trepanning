# -*- coding: utf-8 -*-
require 'trace'
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::ShowEvents < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show trace events we may stop on'
    MIN_ABBREV   = 'ev'.size
    NAME         = File.basename(__FILE__, '.rb')
  end

  # FIXME: this really should be a subcommand of "set trace"
  def run(args)
    event_bitmask = @proc.core.step_events
    if event_bitmask == 0
      msg('No events trapped.')
    else
      msg('Trace events we may stop on:')
      msg("\t" + Trace.bitmask2events(event_bitmask).join(', '))
    end
  end


end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::ShowEvents.new(cmd)
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
  subcommand.run([name])
end
