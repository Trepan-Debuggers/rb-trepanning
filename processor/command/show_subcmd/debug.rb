# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::ShowDebug < Trepan::SubSubcommandMgr
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
  command = Trepan::SubSubcommand::ShowDebug.new(dbgr.core.processor, 
                                                 show_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['show', name]
  show_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/trepanning'
  # Trepan.debug(:set_restart => true)
  command.run(cmd_args)
end
