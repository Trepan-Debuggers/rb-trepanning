# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::SetDebugMacro < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set macro debugging"
    MIN_ABBREV  = 'macro'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(set debug macro)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd  = MockDebugger::setup('set')
  debug_cmd      = Debugger::SubSubcommand::SetDebug.new(dbgr.core.processor, 
                                                        set_cmd)
  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::SetDebugMacro::PREFIX.join('')
  debugx_cmd     = Debugger::SubSubcommand::SetDebugMacro.new(set_cmd.proc, 
                                                              debug_cmd,
                                                              cmd_name)
  # require_relative '../../../../lib rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  debugx_cmd.run([name])
  debugx_cmd.run([name, 'off'])
  debugx_cmd.run([name, 'on'])
end
