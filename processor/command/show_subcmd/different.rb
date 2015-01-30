# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowDifferent < Trepan::ShowBoolSubcommand
    unless defined?(HELP)
        Trepanning::Subcommand.set_name_prefix(__FILE__, self)
        HELP = "Show status of 'set different'"
        MIN_ABBREV   = 'dif'.size
    end

    def run(args)
        case @proc.settings[:different]
         when 'nostack'
            msg("different is nostack.")
        when 'off'
            msg("different is off.")
        when 'on'
            msg("different is on.")
        else
            msg("Unknown value of different %s"  % @proc.settings[:different].inspect)
        end
    end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Trepan::Subcommand::ShowDifferent.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
