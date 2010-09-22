# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::SetMaxStack < Trepan::SubSubcommand
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

  alias save_command save_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Trepan::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                    set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Trepan::SubSubcommand::SetMaxStack::PREFIX.join('')
  subcmd        = Trepan::SubSubcommand::SetMaxStack.new(set_cmd.proc, 
                                                         max_cmd,
                                                         cmd_name)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockTrepan::setup('set')
  max_cmd       = Trepan::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                      set_cmd)
  cmd_name      = Trepan::SubSubcommand::SetMaxStack::PREFIX.join('')
  subcmd        = Trepan::SubSubcommand::SetMaxStack.new(set_cmd.proc,
                                                         max_cmd,
                                                         cmd_name)

  subcmd.run(%w(stack))
  subcmd.run(%w(stack 0))
  subcmd.run(%w(stack 10))
  name = File.basename(__FILE__, '.rb')
  subcmd.summary_help(name)
  puts
  puts '-' * 20

  require_relative '../../../../lib/trepanning'
  dbgr = Trepan.new(:set_restart => true)
  dbgr.debugger
  puts subcmd.save_command
end
