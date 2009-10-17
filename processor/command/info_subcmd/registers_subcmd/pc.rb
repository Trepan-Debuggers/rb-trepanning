# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)
require_relative %w(.. registers)

class Debugger::SubSubcommand::InfoRegistersPc < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM program counter'
    MIN_ABBREV   = 'pc'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
  end

  def run(args)
    msg("VM pc = %d" % @proc.frame.pc_offset)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, info_cmd = MockDebugger::setup('exit')
  testcmdMgr = Debugger::Subcmd.new(info_cmd)
  infox_cmd  = Debugger::SubSubcommand::InfoRegistersPc.new(info_cmd.proc, 
                                                            info_cmd,
                                                            'inforegisters')
  # require_relative %w(.. .. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  infox_cmd.run([])

  def info_cmd.msg(message)
    puts message
  end
  def info_cmd.msg_nocr(message)
    print message
  end
  def info_cmd.errmsg(message)
    puts message
  end
  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
