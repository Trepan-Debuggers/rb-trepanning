# -*- coding: utf-8 -*-
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Debugger::SubSubcommand::SetAuto < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set controls for things with some sort of "automatic" default behavior'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set auto)
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Debugger::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                 set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/rbdbgr'
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
end
