# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Debugger::SubSubcommand::ShowMax < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Show "maximum length" settings'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(show max)
  end

  def run(args)
    super
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  show_cmd = cmds['show']
  command = Debugger::SubSubcommand::ShowMax.new(dbgr.core.processor, 
                                                 show_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['show', name]
  show_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/rbdbgr'
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
end
