# -*- coding: utf-8 -*-
require_relative '../base/subcmd'

class Debugger::Subcommand::ShowBasename < Debugger::ShowBoolSubcommand
  unless defined?(HELP)
    HELP = "Show only file basename in showing file names"
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
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Debugger::Subcommand::ShowBasename.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
