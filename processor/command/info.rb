# -*- coding: utf-8 -*-
require_relative 'base/submgr'

class Debugger::Command::InfoCommand < Debugger::SubcommandMgr
  unless defined?(HELP)
    HELP =
'Generic command for showing things about the program being debugged. 

You can give unique prefix of the name of a subcommand to get
information about just that subcommand.

Type "info" for a list of "info" subcommands and what they do.
Type "help info *" for just a list of "info" subcommands.
'

    ALIASES       = %w(i)
    CATEGORY      = 'status'
    NAME          = File.basename(__FILE__, '.rb')
    SHORT_HELP    = 'Information about debugged program and its environment'
  end
end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
end
