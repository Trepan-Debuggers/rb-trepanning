# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)
require_relative %w(.. auto)

class Debugger::SubSubcommand::ShowAutoIrb < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show if IRB is invoked on debugger stops"
    MIN_ABBREV = 'ir'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show auto irb)
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
  auto_cmd       = Debugger::SubSubcommand::ShowAuto.new(dbgr.core.processor, 
                                                         show_cmd)

  cmd_name       = Debugger::SubSubcommand::ShowAutoIrb::PREFIX.join('')
  autox_cmd      = Debugger::SubSubcommand::ShowAutoIrb.new(show_cmd.proc, auto_cmd,
                                                            cmd_name)
  autox_cmd.run([])
  # name = File.basename(__FILE__, '.rb')
  # autox_cmd.summary_help(name)
end
