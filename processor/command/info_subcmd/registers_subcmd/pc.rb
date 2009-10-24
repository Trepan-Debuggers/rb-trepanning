# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::SubSubcommand::InfoRegistersPc < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM program counter (PC)'
    MIN_ABBREV   = 'pc'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info registers pc)
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
  dbgr, info_cmd = MockDebugger::setup('info')
  testcmdMgr = Debugger::Subcmd.new(info_cmd)
  cmd_name   = Debugger::SubSubcommand::InfoRegistersPc::PREFIX.join('')
  infox_cmd  = Debugger::SubSubcommand::InfoRegistersPc.new(info_cmd.proc, 
                                                            info_cmd,
                                                            cmd_name)
  # require_relative %w(.. .. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  infox_cmd.run([])

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
