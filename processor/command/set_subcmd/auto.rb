# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetAuto < Trepan::SubSubcommandMgr
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
  command = Trepan::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                               set_cmd)
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/trepanning'
  # Trepan.debug
  command.run(cmd_args)
end
