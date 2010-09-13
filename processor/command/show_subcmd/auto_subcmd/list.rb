# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::ShowAutoList < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show running a 'list' command each time we enter the debugger"
    MIN_ABBREV   = 'l'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show auto list)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Debugger::Subcmd.new(show_cmd)
  auto_cmd       = Debugger::SubSubcommand::ShowAuto.new(dbgr.core.processor, 
                                                         show_cmd)

  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::ShowAutoList::PREFIX.join('')
  autox_cmd      = Debugger::SubSubcommand::ShowAutoList.new(show_cmd.proc, auto_cmd,
                                                             cmd_name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  autox_cmd.run([])

end
