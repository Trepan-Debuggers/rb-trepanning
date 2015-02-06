# -*- coding: utf-8 -*-
# Copyright (C) 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetRegister < Trepan::SubSubcommandMgr
    unless defined?(HELP)
        Trepanning::Subcommand.set_name_prefix(__FILE__, self)
        HELP   = <<-EOH
**#{PREFIX.join(' ')}** {*register-name*[,] ...}

Allows setting certain VM registers. *Warning*: these are potentially
dangerous.

See also:
---------
`#{PREFIX.join(' ')} *` for a list of subcommands or `#{PREFIX.join(' ')} *name*`
for help on a particular register subcommand.
    EOH
        SHORT_HELP = 'Set VM registers'
        MIN_ABBREV = 'reg'.size
    end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Trepan::SubSubcommand::SetRegister.new(dbgr.core.processor,
                                               set_cmd)
  cmd_args = Trepan::SubSubcommand::SetRegister::PREFIX
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/trepanning'
  # Trepan.debug
  command.run(cmd_args)
end
