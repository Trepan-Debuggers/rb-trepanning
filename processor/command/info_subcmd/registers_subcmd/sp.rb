# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'
require_relative 'helper'

class Debugger::Subcommand::InfoRegistersSp < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM stack pointer (SP).

The VM uses a stack to store temporary values in computations. For
example to compute "a + b", the values of "a" and "b" are pushed onto
a stack pointed to by SP. Just before the addition is perofrmed, sp(1)
will have the value "a" contians and sp(2) will contain the value of
"b"

See also "info register LFP"'

    MIN_ABBREV   = 'sp'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info registers sp)
  end

  include Registers
  def run(args)
    register_array_index(PREFIX[-1], args)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, info_cmd = MockDebugger::setup('info')
  testcmdMgr = Debugger::Subcmd.new(info_cmd)
  cmd_name   = Debugger::SubSubcommand::InfoRegistersSp::PREFIX.join('')
  infox_cmd  = Debugger::SubSubcommand::InfoRegistersSp.new(info_cmd.proc, 
                                                            info_cmd,
                                                            cmd_name)
  infox_cmd.summary_help(name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  infox_cmd.run([])

  name = File.basename(__FILE__, '.rb')
end
