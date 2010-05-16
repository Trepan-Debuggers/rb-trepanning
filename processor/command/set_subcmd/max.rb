# -*- coding: utf-8 -*-
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Debugger::SubSubcommand::SetMax < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set maximum length for things which may have unbounded size'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set max)
  end

  # def run(args)
  #   puts "foo"
  #   require 'rbdbgr'
  #   Debugger.debug
  #   super
  # end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Debugger::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  command.run(cmd_args)
  require_relative '../../../lib/rbdbgr'
  # Debugger.debug(:set_restart => true)
  command.run(['set', name, 'string', 30])
end
