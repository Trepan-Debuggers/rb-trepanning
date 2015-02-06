# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetAuto < Trepan::SubSubcommandMgr
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP   = <<-EOH
Set controls for things with some sort of "automatic" default behavior.

See "#{PREFIX.join(' ')} *" for a list of subcommands or "#{PREFIX.join(' ')} <name>"
for help on a particular trace subcommand.
    EOH
    SHORT_HELP = 'Set controls for some "automatic" default behaviors'
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Trepan::SubSubcommand::SetAuto.new(dbgr.core.processor,
                                               set_cmd)
  cmd_args = Trepan::SubSubcommand::SetAuto::PREFIX
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/trepanning'
  # Trepan.debug
  command.run(cmd_args)
end
