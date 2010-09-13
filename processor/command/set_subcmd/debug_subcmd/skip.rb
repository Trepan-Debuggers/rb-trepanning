# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::SetDebugSkip < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP        = 'Set debugging of statement skip logic'
    MIN_ABBREV  = 'sk'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(set debug skip)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, dbg_cmd  = MockDebugger::setup('set')
  debug_cmd      = Debugger::SubSubcommand::SetDebug.new(dbgr.core.processor, 
                                                        dbg_cmd)
  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::SetDebugSkip::PREFIX.join('')
  debugx_cmd     = Debugger::SubSubcommand::SetDebugSkip.new(dbg_cmd.proc, 
                                                              debug_cmd,
                                                              cmd_name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  debugx_cmd.run([name])
  debugx_cmd.run([name, 'on'])
  debugx_cmd.run([name, 'off'])
end
