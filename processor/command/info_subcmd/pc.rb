# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::InfoPc < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM program counter'
    MIN_ABBREV   = 'fr'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
  end

  def run(args)
    msg("VM pc %d" % @proc.frame.pc_offset)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::InfoPc.new(cmd)
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
