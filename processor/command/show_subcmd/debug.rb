# -*- coding: utf-8 -*-
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Debugger::SubSubcommand::ShowDebug < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Show internal debugger settings.'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(show debug)
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  show_cmd = cmds['show']
  command = Debugger::SubSubcommand::ShowDebug.new(dbgr.core.processor, 
                                                   show_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['show', name]
  show_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/rbdbgr'
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
end
