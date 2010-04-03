# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::SetTrace < Debugger::SubSubcommandMgr 
  unless defined?(HELP)
    HELP = "Set tracing of various sorts.

The types of tracing include global variables, events from the trace buffer, or printing those events."

    IN_LIST    = true
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX = %w(set trace)
    SHORT_HELP = 'Set tracing of various sorts.'
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  require_relative %w(.. .. hook)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  command = Debugger::SubSubcommand::SetTrace.new(dbgr.core.processor,
                                                  set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative %w(.. .. .. lib rbdbgr)
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
  command.run(['set', name, '*'])
end
