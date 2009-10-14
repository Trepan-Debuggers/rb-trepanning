# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)
require_relative %w(.. base_subsubmgr)

class Debugger::SubSubcommand::ShowAuto < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP = 'Show settings which some sort of "automatic" default behavior'
    NAME = File.basename(__FILE__, '.rb')
  end
end

if __FILE__ == $0
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  show_cmd = cmds['show']
  command = Debugger::SubSubcommand::ShowAuto.new(dbgr.core.processor, 
                                                  show_cmd)
  name = File.basename(__FILE__, '.rb')
  command.run(['show', name])
end
