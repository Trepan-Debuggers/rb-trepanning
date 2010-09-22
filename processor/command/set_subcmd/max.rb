# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetMax < Trepan::SubSubcommandMgr
  unless defined?(HELP)
    HELP   = 'Set maximum length for things which may have unbounded size'
    NAME   = File.basename(__FILE__, '.rb')
    PREFIX = %w(set max)
  end

  # def run(args)
  #   puts "foo"
  #   require 'trepanning'
  #   Trepan.debug
  #   super
  # end
end

if __FILE__ == $0
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Trepan::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                              set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  command.run(cmd_args)
  require_relative '../../../lib/trepanning'
  # Trepan.debug(:set_restart => true)
  command.run(['set', name, 'string', 30])
end
