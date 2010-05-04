# -*- coding: utf-8 -*-
require_relative '../base/subcmd'

class Debugger::Subcommand::SetBasename < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to show only file basename in showing file names"
    IN_LIST    = true
    MIN_ABBREV = 'ba'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set basename)
  end

  alias restore_command restore_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetBasename.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run_show_bool
  subcommand.summary_help(name)

  # require 'rbdbgr'
  # Debugger.debug(:set_restart => true)
  subcommand.run(['set', name])
  subcommand.run(['set', name, 'off'])
  subcommand.run(['set', name, 'on'])
  subcommand.summary_help(name)
  puts
  puts '-' * 20
  puts subcommand.restore_command()

end
