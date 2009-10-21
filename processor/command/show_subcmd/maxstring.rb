# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::ShowMaxstring < Debugger::ShowIntSubcommand
  unless defined?(HELP)
    HELP = 'Show the number of characters in a string before truncating.

Sometimes the string representation of an object is very long. This
setting limits how much of the string representation you want to
see. However if the string has an embedded newline then we will assume
the output is intended to be formated as.'
    MIN_ABBREV   = 'maxs'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFI        = %(show maxstring)
  end

  def run(args)
      run_show_int(:maxstring)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  # require_relative %w(.. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger

  subcommand = Debugger::Subcommand::ShowMaxstring.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run([])
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
