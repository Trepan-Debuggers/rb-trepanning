# -*- coding: utf-8 -*-
require_relative('base_submgr')

class Debugger::Command::SetCommand < Debugger::SubcommandMgr
  unless defined?(HELP)
    HELP =
'Modifies parts of the debugger environment.

You can give unique prefix of the name of a subcommand to get
information about just that subcommand.

Type "set" for a list of "set" subcommands and what they do.
Type "help set *" for just the list of "set" subcommands.
'

    CATEGORY      = 'data'
    MIN_ARGS      = 0
    MAX_ARG       = nil
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Modify parts of the debugger environment'
  end
end

if __FILE__ == $0
  require_relative File.join(%w(.. mock))
  dbgr = MockDebugger.new
  cmds = dbgr.core.processor.instance_variable_get('@commands')
  name = File.basename(__FILE__, '.rb')
  cmd = cmds[name]
  command = Debugger::Command::SetCommand.new(dbgr.core.processor)
  command.run([name])
end
