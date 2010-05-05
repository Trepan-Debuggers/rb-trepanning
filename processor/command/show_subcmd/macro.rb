# -*- coding: utf-8 -*-
require_relative '../base/subcmd'

class Debugger::Subcommand::ShowMacro < Debugger::Subcommand
  unless defined?(HELP)
    HELP = "Show defined macros"
    MIN_ABBREV = 'ma'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show macro)
  end

  def run(args)
    if @proc.macros.empty?
      msg "No macros defined."
    else
      msg columnize_commands(@proc.macros.keys.sort)
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Debugger::Subcommand::ShowMacro.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
