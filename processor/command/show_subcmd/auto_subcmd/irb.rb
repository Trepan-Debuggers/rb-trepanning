# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../auto'

class Trepan::SubSubcommand::ShowAutoIrb < Trepan::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show if IRB is invoked on debugger stops"
    MIN_ABBREV = 'ir'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show auto irb)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Trepan::Subcmd.new(show_cmd)
  auto_cmd       = Trepan::SubSubcommand::ShowAuto.new(dbgr.core.processor, 
                                                       show_cmd)

  cmd_name       = Trepan::SubSubcommand::ShowAutoIrb::PREFIX.join('')
  autox_cmd      = Trepan::SubSubcommand::ShowAutoIrb.new(show_cmd.proc, auto_cmd,
                                                          cmd_name)
  autox_cmd.run([])
  # name = File.basename(__FILE__, '.rb')
  # autox_cmd.summary_help(name)
end
