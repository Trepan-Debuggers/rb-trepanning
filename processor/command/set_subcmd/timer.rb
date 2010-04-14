# -*- coding: utf-8 -*-
require_relative '../base/subsubcmd'

class Debugger::Subcommand::SetTimer < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to show elapsed time between debugger events"
    MIN_ABBREV = 'ti'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set auto irb)
  end

  def run(args)
    super
    if @proc.settings[:timer]
      @proc.cmdloop_prehooks.insert_if_new(-1, 'timer', @proc.timer_hook[1])
      @proc.time_last = Time.now
    else
      @proc.cmdloop_prehooks.delete_by_name('timer')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  require_relative '../../hook'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  subcommand    = Debugger::Subcommand::SetTimer.new(set_cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run_show_bool
  subcommand.summary_help(name)

  # require 'rbdbgr'
  # Debugger.debug(:set_restart => true)
  subcommand.run(['set', name])
  subcommand.run(['set', name, 'off'])
  subcommand.run(['set', name, 'on'])

end
