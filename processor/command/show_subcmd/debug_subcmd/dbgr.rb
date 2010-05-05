# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::ShowDebugDbgr < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP        = "Show debugging the debugger"
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(show debug dbgr)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd  = MockDebugger::setup('show')
  debug_cmd       = Debugger::SubSubcommand::ShowDebug.new(dbgr.core.processor, 
                                                           show_cmd)

  # FIXME: remove the 'join' below
  cmd_name        = Debugger::SubSubcommand::ShowDebugDbgr::PREFIX.join('')
  debugx_cmd      = Debugger::SubSubcommand::ShowDebugDbgr.new(show_cmd.proc, 
                                                               debug_cmd,
                                                               cmd_name)

  debugx_cmd.run([])
end
