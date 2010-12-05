# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowBasename < Trepan::ShowBoolSubcommand
  unless defined?(HELP)
    HELP       = "Show only file basename in showing file names"
    MIN_ABBREV = 'ba'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %W(show #{NAME})
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Trepan::Subcommand::ShowBasename.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
