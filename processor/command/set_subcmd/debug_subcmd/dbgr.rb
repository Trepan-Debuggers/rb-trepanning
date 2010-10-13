# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetDebugDbgr < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP        = 'set debug dbgr [on|off]

Facilitates debugging the debugger. Global variables $trepan_cmdproc
and $trepan_frame are set to the current values of @frame and self
when the command processor was entered.  '

    MIN_ABBREV  = 'db'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %W(set debug #{NAME})
    SHORT_HELP  = 'Set debugging debugger'
  end

  def run(args)
    super
    @proc.cmdloop_prehooks.insert_if_new(-1, *@proc.debug_dbgr_hook)
    @proc.debug_dbgr_hook[1].call
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, dbg_cmd  = MockDebugger::setup('set')
  debug_cmd      = Trepan::SubSubcommand::SetDebug.new(dbgr.core.processor, 
                                                        dbg_cmd)
  # FIXME: remove the 'join' below
  cmd_name       = Trepan::SubSubcommand::SetDebugDbgr::PREFIX.join('')
  debugx_cmd     = Trepan::SubSubcommand::SetDebugDbgr.new(dbg_cmd.proc, 
                                                           debug_cmd,
                                                           cmd_name)
  # require_relative '../.././../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  debugx_cmd.run([name])
  debugx_cmd.run([name, 'on'])
  debugx_cmd.run([name, 'off'])
  puts '-' * 10
  puts debugx_cmd.save_command
end
