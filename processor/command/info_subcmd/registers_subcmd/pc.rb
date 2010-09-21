# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::InfoRegistersPc < Trepan::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM program counter (PC).

The VM program is an offset into the instruction sequence for the next
VM instruction in the sequence to be executed. 

See also "info disassemble" and "info registers".'
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
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, info_cmd = MockDebugger::setup('info')
  testcmdMgr = Trepan::Subcmd.new(info_cmd)
  cmd_name   = Trepan::SubSubcommand::InfoRegistersPc::PREFIX.join('')
  infox_cmd  = Trepan::SubSubcommand::InfoRegistersPc.new(info_cmd.proc, 
                                                          info_cmd,
                                                          cmd_name)
  # require_relative '../../../../rbdbgr'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  infox_cmd.run([])

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
