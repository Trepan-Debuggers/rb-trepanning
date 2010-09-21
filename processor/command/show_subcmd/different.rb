# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowDifferent < Trepan::ShowBoolSubcommand
  unless defined?(HELP)
    HELP = "Show status of 'set different'"
    MIN_ABBREV   = 'dif'.size
    PREFIX       = %w(show different)
    NAME         = File.basename(__FILE__, '.rb')
  end

  def run(args)
    if 'nostack' == @proc.settings[:different]
      msg("different is nostack.")
    else
      super
    end
  end


end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Trepan::Subcommand::ShowDifferent.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
