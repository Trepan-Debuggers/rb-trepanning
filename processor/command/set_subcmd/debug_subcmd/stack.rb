# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::SubSubcommand::SetDebugStack < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set display of complete stack including possibly setup stack from rbdbgr"
    MIN_ABBREV  = 'st'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(set debug stack)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd  = MockDebugger::setup('set')
  debug_cmd      = Debugger::SubSubcommand::SetDebug.new(dbgr.core.processor, 
                                                        set_cmd)
  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::SetDebugStack::PREFIX.join('')
  debugx_cmd     = Debugger::SubSubcommand::SetDebugStack.new(set_cmd.proc, 
                                                              debug_cmd,
                                                              cmd_name)
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  debugx_cmd.run([name])
  debugx_cmd.run([name, 'off'])
  debugx_cmd.run([name, 'on'])
end
