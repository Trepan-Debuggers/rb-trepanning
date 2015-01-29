# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetDifferent < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
#{PREFIX.join(' ')} [on|off|nostack]

Set to make sure 'next' or 'step' moves to a new position.

Due to the interpretive, expression-oriented nature of the Ruby
Language and implementation, each line often may contain many possible
stopping points with possibly different event type. In a debugger it
is sometimes desirable to continue but stop only when the position
next changes.

Setting 'different' to on will cause each 'step' or 'next' command to
stop at a different position.

Note though that the notion of different does take into account stack
nesting. So in ARGV.map {|arg| arg.to_i} you get a stop before ARGV as
well as one in the block.

If you to ignore stopping at added nesting levels, there are two
possibilities. 'set step nostack' will ignore stack nestings levels on
a given line. Also you can use 'next', but that also stepping into
functions on different lines to also be skipped.

See also 'step', 'next' which have suffixes '+' and '-' which
override this setting.
    EOH

    IN_LIST      = true
    MIN_ABBREV   = 'dif'.size
    SHORT_HELP   = "Set to make sure 'next/step' move to a new position."
  end

  completion %w(on off nostack)

    def run(args)
        if args.size == 3 && 'nostack' == args[2]
            @proc.settings[:different] = 'nostack'
            msg("different is nostack.")
        else
            super
        end
        @proc.different_pos = @proc.settings[:different]
    end

end

if __FILE__ == $0
    # Demo it.
    require_relative '../../mock'
    name = File.basename(__FILE__, '.rb')

    # FIXME: DRY the below code
    dbgr, cmd = MockDebugger::setup('set')
    subcommand = Trepan::Subcommand::SetDifferent.new(cmd)
    testcmdMgr = Trepan::Subcmd.new(subcommand)

    subcommand.run_show_bool
    # subcommand.summary_help(name)
    puts
    puts '-' * 20
    puts subcommand.save_command
end
