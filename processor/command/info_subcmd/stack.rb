# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoStack < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = 'Same thing as "backtrace"'
    MIN_ABBREV   = 'st'.size
    NEED_STACK   = true
   end

  def run(args)
    @proc.commands['backtrace'].run(['backtrace'] + args[2..-1])
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoStack, false)
  cmd.run(cmd.prefix)
end
