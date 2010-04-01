# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::SubSubcommand::ShowTraceBuffer < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show tracing buffer status"
    MIN_ABBREV   = 'b'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show trace buffer)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Debugger::Subcmd.new(show_cmd)
  trace_cmd      = Debugger::SubSubcommand::ShowTrace.new(dbgr.core.processor, 
                                                          show_cmd)

  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::ShowTraceBuffer::PREFIX.join('')
  tb_cmd         = Debugger::SubSubcommand::ShowTraceBuffer.new(show_cmd.proc, 
                                                                trace_cmd,
                                                                cmd_name)
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  tb_cmd.run([])

end
