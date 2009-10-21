# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::SubSubcommand::ShowDebugStack < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP        = "Show complete stack including possibly setup stack from rbdbgr"
    MIN_ABBREV  = 'st'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(show debug stack)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd  = MockDebugger::setup('show')
  debug_cmd       = Debugger::SubSubcommand::ShowDebug.new(dbgr.core.processor, 
                                                           show_cmd)

  # FIXME: remove the 'join' below
  cmd_name        = Debugger::SubSubcommand::ShowDebugStack::PREFIX.join('')
  debugx_cmd      = Debugger::SubSubcommand::ShowDebugStack.new(show_cmd.proc, 
                                                                debug_cmd,
                                                                cmd_name)

  debugx_cmd.run([])
end
