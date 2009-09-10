# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)
require_relative %w(.. base_subsubmgr)

class Debugger::SubSubcommand::ShowTrace < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP = 'Show aspects regarding tracing'

    MIN_ARGS      = 0
    MAX_ARG       = nil
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Show aspects regarding tracing'
  end
end

if __FILE__ == $0
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new(nil)
  cmds = dbgr.core.processor.commands
  show_cmd = cmds['show']
  command = Debugger::SubSubcommand::ShowTrace.new(dbgr.core.processor, 
                                                   show_cmd)
  name = File.basename(__FILE__, '.rb')
  command.run(['show', name])
end
