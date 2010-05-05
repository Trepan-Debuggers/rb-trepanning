# -*- coding: utf-8 -*-
require_relative '../base/subcmd'

class Debugger::Subcommand::ShowAlias < Debugger::Subcommand
  unless defined?(HELP)
    HELP = "show alias [NAME1 NAME2 ...] 

If aliases names are given, show their definition. If left blank, show
all aliases"

    MIN_ABBREV = 'al'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show alias)
    SHORT_HELP = "Show defined aliases"
  end

  def run(args)
    if @proc.aliases.empty?
      msg "No aliases defined."
    else
      msg columnize_commands(@proc.aliases.keys.sort)
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Debugger::Subcommand::ShowAlias.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
