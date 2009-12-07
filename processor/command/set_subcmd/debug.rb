# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::SetDebug < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set internal debugger settings'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set debug)
  end
end

if __FILE__ == $0
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Debugger::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                  set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative %w(.. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  command.run(cmd_args)
end
