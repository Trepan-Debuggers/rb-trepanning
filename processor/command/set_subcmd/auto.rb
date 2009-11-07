# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::SetAuto < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set controls for things with some sort of "automatic" default behavior.'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set auto)
  end
end

if __FILE__ == $0
  require_relative %w(.. .. mock)
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Debugger::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                  set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative %w(.. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  command.run(cmd_args)
end
