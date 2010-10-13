# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowMacro < Trepan::Subcommand
  unless defined?(HELP)
    HELP = "Show defined macros"
    MIN_ABBREV = 'ma'.size
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

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Trepan::Subcommand::ShowMacro.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  subcommand.run(%W(show #{name}))
  subcommand.run(%W(show #{name} u foo))
end
