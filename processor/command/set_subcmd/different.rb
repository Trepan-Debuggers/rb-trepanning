# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetDifferent < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to make sure 'next/step' move to a new position.

Due to the interpretive, expression-oriented nature of the Ruby
Language and implementation, each line often may contain many possible
stopping points with possibly different event type. In a debugger it
is sometimes desirable to continue but stop only when the position
next changes.

Setting 'different' to on will cause each 'step' or 'next' command to
stop at a different position.

Note though that the notion of different does take into account stack
nesting. So in ARGV.map {|arg| arg.to_i} you get a stop before ARGV as
well as one in the block -- but only one in the block since subsequent
steps will be at the same position. If you to ignore stopping at added
nesting levels, use 'next'.

See also 'step', 'next' which have suffixes '+' and '-' which
override this setting."

    IN_LIST      = true
    MIN_ABBREV   = 'dif'.size
    NAME         = File.basename(__FILE__, '.rb')
  end

  def run(args)
    super
    @proc.different_pos = @proc.settings[:different]
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::SetDifferent.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  subcommand.run_show_bool
  subcommand.summary_help(name)
end
