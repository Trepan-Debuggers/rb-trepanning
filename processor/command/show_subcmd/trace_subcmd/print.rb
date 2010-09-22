# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::ShowTracePrint < Trepan::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show tracing print status"
    MIN_ABBREV   = 'p'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show trace buffer)
    SHORT_HELP   = "Show tracing print status"
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
  trace_cmd      = Trepan::SubSubcommand::ShowTrace.new(dbgr.core.processor, 
                                                        show_cmd)

  # FIXME: remove the 'join' below
  cmd_name       = Trepan::SubSubcommand::ShowTracePrint::PREFIX.join('')
  tb_cmd         = Trepan::SubSubcommand::ShowTracePrint.new(show_cmd.proc, 
                                                             trace_cmd,
                                                             cmd_name)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  tb_cmd.run([])

end
