# -*- coding: utf-8 -*-
require_relative('base_submgr')

class Debugger::Command::ShowCommand < Debugger::SubcommandMgr
  unless defined?(HELP)
    HELP =
'Generic command for showing things about the debugger.  You can
give unique prefix of the name of a subcommand to get information
about just that subcommand.

Type "show" for a list of "show" subcommands and what they do.
Type "help show *" for just a list of "show" subcommands.'

    CATEGORY      = 'status'
    MIN_ARGS      = 0
    MAX_ARG       = nil
    NAME          = File.basename(__FILE__, '.rb')
    NAME_ALIASES  = [NAME]
    NEED_STACK    = false
    SHORT_HELP    = 'Show parts of the debugger environment'
  end
end

if __FILE__ == $0
  require_relative File.join(%w(.. mock))
  dbgr = MockDebugger.new
  cmds = dbgr.core.processor.instance_variable_get('@commands')
  name = File.basename(__FILE__, '.rb')
  cmd = cmds[name]
  command = Debugger::Command::ShowCommand.new(dbgr.core.processor)
  command.run([name])
end
