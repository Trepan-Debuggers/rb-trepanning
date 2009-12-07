# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::Subcommand::SetAutoIrb < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set to automatically go into irb each time we enter the debugger"
    MIN_ABBREV = 'ir'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set auto irb)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  auto_cmd      = Debugger::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                       set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Debugger::Subcommand::SetAutoIrb::PREFIX.join('')
  autox_cmd     = Debugger::SubSubcommand::SetAutoIrb.new(set_cmd.proc, 
                                                          auto_cmd,
                                                          cmd_name)
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  subcmd_name = Debugger::Subcommand::SetAutoIrb::PREFIX[1..-1].join('')
  autox_cmd.run([subcmd_name])
  autox_cmd.run([subcmd_name, 'off'])

end
