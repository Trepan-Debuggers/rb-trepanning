# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::ShowAutoirb < Debugger::ShowBoolSubcommand
  unless defined?(HELP)
    HELP = "Show if IRB is invoked on debugger stops"
    MIN_ABBREV = 'autoi'.size
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
  subcommand = Debugger::Subcommand::ShowAutoirb.new(cmd)
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
