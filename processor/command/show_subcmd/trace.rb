# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::ShowTrace < Debugger::SubSubcommandMgr 

  unless defined?(HELP)
    HELP = "Show event tracing printing"
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show trace)
    SHORT_HELP = HELP
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  show_cmd = cmds['show']
  command = Debugger::SubSubcommand::ShowTrace.new(dbgr.core.processor, 
                                                   show_cmd)

  name = File.basename(__FILE__, '.rb')
  cmd_args = ['show', name]
  command.run(cmd_args)
end
