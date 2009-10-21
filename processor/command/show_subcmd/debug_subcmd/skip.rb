# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::SubSubcommand::ShowDebugSkip < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP        = 'Show debugging of statement skip logic'
    MIN_ABBREV  = 'st'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(show debug skip)
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
  cmd_name        = Debugger::SubSubcommand::ShowDebugSkip::PREFIX.join('')
  debugx_cmd      = Debugger::SubSubcommand::ShowDebugSkip.new(show_cmd.proc, 
                                                               debug_cmd,
                                                               cmd_name)

  debugx_cmd.run([])
end
