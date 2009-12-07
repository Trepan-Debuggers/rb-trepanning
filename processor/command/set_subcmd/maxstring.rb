# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetMaxstring < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Set maximum number of characters in a string before truncating.

Sometimes the string representation of an object is very long. This
setting limits how much of the string representation you want to
see. However if the string has an embedded newline then we will assume
the output is intended to be formated as is.
'
    IN_LIST      = true
    MIN_ABBREV   = 'maxs'.size
    NAME         = File.basename(__FILE__, '.rb')
    DEFAULT_MIN  = 10
    PREFIX       = %w(set maxstring)
  end

  def run(args)
    args = %W(#{DEFAULT_MIN}) if args.empty?
    run_set_int(args.join(' '),
                "The 'set maxstr' command requires number at least 10", 
                DEFAULT_MIN, nil)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetMaxstring.new(cmd)
  subcommand.run([])
  subcommand.run(%w(0))
  subcommand.run(%w(20))
  subcommand.run(%w(100))
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
