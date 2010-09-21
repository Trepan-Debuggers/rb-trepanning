# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowEvents < Trepan::Subcommand
  unless defined?(HELP)
    HELP         = 'Show trace events we may stop on.'
    MIN_ABBREV   = 'ev'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show events)
  end

  # FIXME: this really should be a subcommand of "set trace"
  def run(args)
    step_events_list = @proc.core.step_events_list
    if step_events_list
      msg 'Trace events we may stop on:'
      msg "\t" + step_events_list
    else
      msg 'No events trapped.'
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Trepan::Subcommand::ShowEvents.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  puts
  subcommand.run([name])
end
