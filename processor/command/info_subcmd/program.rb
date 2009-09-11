# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::InfoProgram < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Information about debugged program and its environment'
    MIN_ABBREV   = 'li'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    SHORT_HELP   = HELP
  end

  def run(args)
    frame = @proc.frame
    msg("Program stop event: %s" % @proc.core.event)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::InfoProgram.new(cmd)
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
