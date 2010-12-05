# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::SetMaxStack < Trepan::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Set number of backtrace lines the debugger will show'
    DEFAULT_MIN  = 3
    MIN_ABBREV   = 'sta'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set max stack)
  end

  def run(args)
    args.shift
    args = %W(#{DEFAULT_MIN}) if args.empty?
    run_set_int(args.join(' '),
                "The '#{PREFIX.join(' ')}'command requires number at least #{DEFAULT_MIN}", 
                DEFAULT_MIN, nil)
  end

  alias save_command save_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../max'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetMax,
                                   Trepan::SubSubcommand::SetMaxStack, false)
  prefix_run = cmd.prefix[1..-1]
  cmd.run(prefix_run)
  cmd.run(prefix_run + %w(0))
  cmd.run(prefix_run + %w(10))
  puts cmd.save_command
end
