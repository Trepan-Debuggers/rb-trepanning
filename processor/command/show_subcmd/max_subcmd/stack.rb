# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Debugger::Subcommand::ShowMaxStack < Debugger::ShowIntSubSubcommand
  unless defined?(HELP)
    HELP         = 'Show the number of backtrace lines the debugger will show'
    MIN_ABBREV   = 'sta'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show max stack)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Debugger::Subcmd.new(show_cmd)
  max_cmd        = Debugger::SubSubcommand::ShowMax.new(dbgr.core.processor, 
                                                        show_cmd)
  
  cmd_name       = Debugger::SubSubcommand::ShowMaxStack::PREFIX.join('')
  maxx_cmd       = Debugger::SubSubcommand::ShowMaxStack.new(show_cmd.proc,
                                                             max_cmd,
                                                             cmd_name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  puts max_cmd.summary_help(maxx_cmd)
  maxx_cmd.run([])
end
