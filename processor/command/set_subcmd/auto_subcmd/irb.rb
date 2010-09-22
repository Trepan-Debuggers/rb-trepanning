# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::SetAutoIrb < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set to automatically go into irb each time we enter the debugger"
    MIN_ABBREV = 'ir'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set auto irb)
  end

  def run(args)
    super
    if @proc.settings[:autoirb]
      @proc.cmdloop_prehooks.insert_if_new(-1, *@proc.autoirb_hook)
    else
      @proc.cmdloop_prehooks.delete_by_name('autoirb')
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
  dbgr, set_cmd = MockTrepan::setup('set')
  auto_cmd      = Trepan::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                     set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Trepan::Subcommand::SetAutoIrb::PREFIX.join('')
  autox_cmd     = Trepan::SubSubcommand::SetAutoIrb.new(set_cmd.proc, 
                                                        auto_cmd,
                                                        cmd_name)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  set_cmd.proc.hook_initialize(set_cmd.proc.commands)
  subcmd_name = Trepan::Subcommand::SetAutoIrb::PREFIX[1..-1].join('')
  autox_cmd.run([subcmd_name])
  autox_cmd.run([subcmd_name, 'off'])
  puts autox_cmd.save_command.join("\n")

end
