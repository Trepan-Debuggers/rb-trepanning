# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::Subcommand::ShowMaxWidth < Debugger::ShowIntSubSubcommand
  unless defined?(HELP)
    HELP = 'Show the number of characters the debugger thinks are in a line.'
    MIN_ABBREV   = 'wid'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show max width)
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
  cmd_name       = Debugger::SubSubcommand::ShowMaxWidth::PREFIX.join('')
  maxx_cmd       = Debugger::SubSubcommand::ShowMaxWidth.new(show_cmd.proc,
                                                             max_cmd,
                                                             cmd_name)
  
  name = File.basename(__FILE__, '.rb')
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  puts max_cmd.summary_help(maxx_cmd)
  maxx_cmd.run([])
end
