# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::ShowMaxList < Trepan::ShowIntSubSubcommand
  unless defined?(HELP)
    HELP = 'Show the number of source file lines to list.'
    MIN_ABBREV   = 'lis'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show max list)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Trepan::Subcmd.new(show_cmd)
  max_cmd        = Trepan::SubSubcommand::ShowMax.new(dbgr.core.processor, 
                                                      show_cmd)
  cmd_ary        = Trepan::SubSubcommand::ShowMaxList::PREFIX
  cmd_name       = cmd_ary.join('')
  subcmd         = Trepan::SubSubcommand::ShowMaxList.new(show_cmd.proc,
                                                          max_cmd,
                                                          cmd_name)
  prefix_run = cmd_ary[1..-1]
  subcmd.run(prefix_run)
  
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  puts subcmd.summary_help(name)
  puts
  puts '-' * 20
end
