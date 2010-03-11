# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetBtlimit < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Set number of backtrace lines the debugger will show'
    MIN_ABBREV   = 'btl'.size
    NAME         = File.basename(__FILE__, '.rb')
  end

  def run(args)
    if args.size >= 3 
      run_set_int(args[2..-1].join(' '),
                  "The 'btlimit' command requires a line width", 
                  0, nil)
    else
      errmsg "Too few arguments - the 'btlimit' command requires a number"
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetBTLimit.new(cmd)
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
  subcommand.run(%w(20))
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
