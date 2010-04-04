# -*- coding: utf-8 -*-
require_relative '../base/subcmd'

class Debugger::Subcommand::SetBasename < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to show only file basename in showing file names"
    IN_LIST    = true
    MIN_ABBREV = 'ba'.size
    NAME       = File.basename(__FILE__, '.rb')
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetBasename.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run_show_bool
  subcommand.summary_help(name)
end
