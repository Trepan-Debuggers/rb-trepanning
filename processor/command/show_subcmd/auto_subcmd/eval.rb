# -*- coding: utf-8 -*-
require_relative %w(.. .. base_subsubcmd)
require_relative %w(.. auto)

class Debugger::SubSubcommand::ShowAutoEval < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show evaluation of unrecognized debugger commands"
    MIN_ABBREV   = 'ev'.size
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = HELP
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Debugger::Subcmd.new(show_cmd)
  auto_cmd       = Debugger::SubSubcommand::ShowAuto.new(dbgr.core.processor, 
                                                         show_cmd)

  autoeval_cmd = Debugger::SubSubcommand::ShowAutoEval.new(show_cmd.proc, auto_cmd)
  # require_relative %w(.. .. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  autoeval_cmd.run([])

  def show_cmd.msg(message)
    puts message
  end
  def show_cmd.msg_nocr(message)
    print message
  end
  def show_cmd.errmsg(message)
    puts message
  end
  name = File.basename(__FILE__, '.rb')
end
