# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::ShowMaxWidth < Trepan::ShowIntSubSubcommand
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
  testcmdMgr     = Trepan::Subcmd.new(show_cmd)
  max_cmd        = Trepan::SubSubcommand::ShowMax.new(dbgr.core.processor, 
                                                      show_cmd)
  cmd_name       = Trepan::SubSubcommand::ShowMaxWidth::PREFIX.join('')
  maxx_cmd       = Trepan::SubSubcommand::ShowMaxWidth.new(show_cmd.proc,
                                                           max_cmd,
                                                           cmd_name)
  
  name = File.basename(__FILE__, '.rb')
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  puts max_cmd.summary_help(maxx_cmd)
  maxx_cmd.run([])
end
