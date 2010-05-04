# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::Subcommand::SetMaxStack < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Set number of backtrace lines the debugger will show'
    DEFAULT_MIN  = 3
    MIN_ABBREV   = 'sta'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set max stack)
  end

  def run(args)
    args.shift
    args = %W(#{DEFAULT_MIN}) if args.empty?
    run_set_int(args.join(' '),
                "The 'set maximum stack' command requires number at least #{DEFAULT_MIN}", 
                DEFAULT_MIN, nil)
  end

  alias restore_command restore_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Debugger::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                      set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Debugger::SubSubcommand::SetMaxStack::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetMaxStack.new(set_cmd.proc, 
                                                           max_cmd,
                                                           cmd_name)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Debugger::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                      set_cmd)
  cmd_name      = Debugger::SubSubcommand::SetMaxStack::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetMaxStack.new(set_cmd.proc,
                                                           max_cmd,
                                                           cmd_name)

  subcmd.run(%w(stack))
  subcmd.run(%w(stack 0))
  subcmd.run(%w(stack 10))
  name = File.basename(__FILE__, '.rb')
  subcmd.summary_help(name)
  puts
  puts '-' * 20

  require_relative '../../../../lib/rbdbgr'
  dbgr = Debugger.new(:set_restart => true)
  dbgr.debugger
  puts subcmd.restore_command()
end
