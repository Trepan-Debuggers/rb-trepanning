# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetDebugMacro < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set macro debugging"
    MIN_ABBREV  = 'macro'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(set debug macro)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd  = MockTrepan::setup('set')
  debug_cmd      = Trepan::SubSubcommand::SetDebug.new(dbgr.core.processor, 
                                                        set_cmd)
  # FIXME: remove the 'join' below
  cmd_name       = Trepan::SubSubcommand::SetDebugMacro::PREFIX.join('')
  debugx_cmd     = Trepan::SubSubcommand::SetDebugMacro.new(set_cmd.proc, 
                                                            debug_cmd,
                                                            cmd_name)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  debugx_cmd.run([name])
  debugx_cmd.run([name, 'off'])
  debugx_cmd.run([name, 'on'])
end
