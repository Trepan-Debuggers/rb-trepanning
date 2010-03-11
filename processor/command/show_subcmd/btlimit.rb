# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::ShowBtlimit < Debugger::ShowIntSubcommand
  unless defined?(HELP)
    HELP         = 'Show the number of backtrace lines the debugger will show'
    MIN_ABBREV   = 'btl'.size
    NAME         = File.basename(__FILE__, '.rb')
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Debugger::Subcommand::ShowBtlimit.new(cmd)
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
  subcommand.run([])
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
