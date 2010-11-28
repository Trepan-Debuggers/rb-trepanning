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
    PREFIX       = %W(info registers #{NAME})
  end

  def run(args)
    msg("VM pc = %d" % @proc.frame.pc_offset)
  end
end

if __FILE__ == $0
  require_relative '../../../mock'
  require_relative '../registers'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::InfoRegisters,
                                   Trepan::SubSubcommand::InfoRegistersPc,
                                   false)
  cmd.run([])
end
