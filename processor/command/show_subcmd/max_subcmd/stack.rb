# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::ShowMaxStack < Trepan::ShowIntSubSubcommand
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
  testcmdMgr     = Trepan::Subcmd.new(show_cmd)
  max_cmd        = Trepan::SubSubcommand::ShowMax.new(dbgr.core.processor, 
                                                      show_cmd)
  
  cmd_name       = Trepan::SubSubcommand::ShowMaxStack::PREFIX.join('')
  maxx_cmd       = Trepan::SubSubcommand::ShowMaxStack.new(show_cmd.proc,
                                                           max_cmd,
                                                           cmd_name)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  puts max_cmd.summary_help(maxx_cmd)
  puts
  maxx_cmd.run([])
end
