# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetSubstitute < Trepan::SubSubcommandMgr
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP   = 'Set instruction sequence-to-filename mapping'
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, set_cmd = MockDebugger::setup('set')
  command = Trepan::SubSubcommand::SetSubstitute.new(dbgr.core.processor, 
                                                       set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/trepanning'
  # Trepan.debug
  command.run(cmd_args)
end
