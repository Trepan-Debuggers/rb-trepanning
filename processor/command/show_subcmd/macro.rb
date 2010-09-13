# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Debugger::Subcommand::ShowMacro < Debugger::Subcommand
  unless defined?(HELP)
    HELP = "Show defined macros"
    MIN_ABBREV = 'ma'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show macro)
  end

  def run(args)
    if args.size > 2
      args[2..-1].each do |macro_name|
        if @proc.macros.member?(macro_name)
          msg "%s: %s" % [macro_name, @proc.macros[macro_name]]
        else
          msg "%s is not a defined macro" % macro_name
        end
      end
    elsif @proc.macros.empty?
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
  subcommand.run(%w(show macro))
  subcommand.run(%w(show macro u foo))
end
