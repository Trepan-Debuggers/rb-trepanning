# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Debugger::SubSubcommand::SetSubstitute < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set instruction sequence-to-filename mapping'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set substitute)
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, set_cmd = MockDebugger::setup('set')
  command = Debugger::SubSubcommand::SetSubstitute.new(dbgr.core.processor, 
                                                       set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/rbdbgr'
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
end
