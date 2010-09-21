# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ReloadCommand < Trepan::Subcommand
  unless defined?(HELP)
    HELP         = 'Reload debugger commmands from debugger directories'
    MIN_ABBREV   = 'co'.size # Note we have "info file"
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(reload command)
  end

  def run(args)
    @proc.load_cmds_initialize
    msg('Debugger commands reloaded.')
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd  = MockDebugger::setup('reload')
  subcommand = Trepan::Subcommand::ReloadCommand.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
