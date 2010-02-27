# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::SetSubstitute < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set instruction sequence-to-filename mapping'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set substitute)
  end
end

if __FILE__ == $0
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Debugger::SubSubcommand::SetSubstitute.new(dbgr.core.processor, 
                                                       set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative %w(.. .. .. lib rbdbgr)
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
end
