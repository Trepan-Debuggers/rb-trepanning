# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

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
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::SetBasename.new(cmd)
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
  subcommand.summary_help(name)
end
