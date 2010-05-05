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
    if args.size > 2
      args[2..-1].each do |alias_name|
        if @proc.aliases.member?(alias_name)
          msg "%s: %s" % [alias_name, @proc.aliases[alias_name]]
        else
          msg "%s is not a defined alias" % alias_name
        end
      end
    elsif @proc.aliases.empty?
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

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  puts 
  subcommand.run(%w(show alias))
  subcommand.run(%w(show alias u foo))
end
