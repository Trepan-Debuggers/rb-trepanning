# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetBasename < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to show only file basename in showing file names"
    IN_LIST    = true
    MIN_ABBREV = 'ba'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set basename)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Trepan::Subcommand::SetBasename.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  subcommand.run_show_bool
  subcommand.summary_help(name)

  # require 'trepanning'
  # Trepan.debug(:set_restart => true)
  subcommand.run(['set', name])
  subcommand.run(['set', name, 'off'])
  subcommand.run(['set', name, 'on'])
  subcommand.summary_help(name)
  puts
  puts '-' * 20
  puts subcommand.save_command

end
