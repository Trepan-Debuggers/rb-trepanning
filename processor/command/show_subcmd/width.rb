# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::ShowWidth < Debugger::ShowIntSubcommand
  unless defined?(HELP)
    HELP = 'Show the number of characters the debugger thinks are in a line'
    MIN_ABBREV   = 'wid'.size
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = HELP
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::ShowAutoeval.new(cmd)
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
  subcommand.run_show_int
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
