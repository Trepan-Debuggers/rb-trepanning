# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowAliases < Trepan::Subcommand
  Trepanning::Subcommand.set_name_prefix(__FILE__, self)
  unless defined?(HELP)
    HELP         = <<-EOH
#{CMD} [NAME1 NAME2 ...] 

If aliases names are given, show their definition. If left blank, show
all alias names
    EOH
    MIN_ABBREV = 'al'.size
    SHORT_HELP = "Show defined aliases"
  end

  def complete(prefix)
    Trepan::Complete.complete_token(@proc.aliases.keys, prefix)
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
      section "List of alias names currently defined:"
      msg columnize_commands(@proc.aliases.keys.sort)
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::ShowAlias)
  cmd.run(cmd.prefix + %w(u foo))
end
