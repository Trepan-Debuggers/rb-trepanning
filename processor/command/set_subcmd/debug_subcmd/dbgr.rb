# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::SubSubcommand::SetDebugDbgr < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP        = 'Set debugging debugger'
    MIN_ABBREV  = 'db'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(set debug skip)
  end

  def run(args)
    $rbdbgr_cmdproc  = @proc
    $rbdbgr_frame    = @proc.frame
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, dbg_cmd  = MockDebugger::setup('set')
  debug_cmd      = Debugger::SubSubcommand::SetDebug.new(dbgr.core.processor, 
                                                        dbg_cmd)
  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::SetDebugDbgr::PREFIX.join('')
  debugx_cmd     = Debugger::SubSubcommand::SetDebugDbgr.new(dbg_cmd.proc, 
                                                              debug_cmd,
                                                              cmd_name)
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  debugx_cmd.run([name])
  debugx_cmd.run([name, 'on'])
  debugx_cmd.run([name, 'off'])
end
