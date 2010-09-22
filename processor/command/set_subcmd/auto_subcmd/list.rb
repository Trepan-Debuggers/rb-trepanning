# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::SetAutoList < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set to run a 'list' command each time we enter the debugger"
    MIN_ABBREV = 'l'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set auto list)
    SHORT_HELP = "Set running a 'list' command each time we enter the debugger"
  end

  def run(args)
    super
    if @proc.settings[:autolist]
      @proc.cmdloop_prehooks.insert_if_new(10, *@proc.autolist_hook)
    else
      @proc.cmdloop_prehooks.delete_by_name('autolist')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  require_relative '../../../hook'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  testcmdMgr    = Trepan::Subcmd.new(set_cmd)
  auto_cmd      = Trepan::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                     set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Trepan::Subcommand::SetAutoList::PREFIX.join('')
  autox_cmd     = Trepan::SubSubcommand::SetAutoList.new(set_cmd.proc, 
                                                         auto_cmd,
                                                         cmd_name)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  set_cmd.proc.hook_initialize(set_cmd.proc.commands)
  subcmd_name = Trepan::Subcommand::SetAutoList::PREFIX[1..-1].join('')
  autox_cmd.run([subcmd_name])
  autox_cmd.run([subcmd_name, 'off'])
  puts '-' * 10
  puts autox_cmd.save_command
end
