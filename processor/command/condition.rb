# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../breakpoint'
require_relative '../../app/breakpoint'
require_relative '../../app/condition'

class Trepan::Command::ConditionCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
**#{NAME}** *bp-number* *condition*

*bp-number* is a breakpoint number.  *condition* is an expression which
must evaluate true before the breakpoint is honored.  If *condition*
is absent, any existing condition is removed; i.e., the breakpoint is
made unconditional.

Examples:
---------

    #{NAME} 5 x > 10  # Breakpoint 5 now has condition x > 10
    #{NAME} 5         # Remove above condition

See also:
--------

`break`, `enable` and `disable`.
    HELP

    ALIASES       = %w(cond)
    CATEGORY      = 'breakpoints'
    MIN_ARGS      = 1
    NEED_STACK    = false
    SHORT_HELP    = 'Specify breakpoint number N to break only if COND is true'
  end

  include Trepan::Condition

  def run(args)
    bpnum = @proc.get_an_int(args[1])
    bp = @proc.breakpoint_find(bpnum)
    return unless bp

    if args.size > 2
      condition = args[2..-1].join(' ')
      return unless valid_condition?(condition)
    else
      condition = 'true'
      msg('Breakpoint %d is now unconditional.' % bp.id)
    end
    bp.condition = condition
  end
end

if __FILE__ == $0
  require 'thread_frame'
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.proc.frame_setup(RubyVM::Frame::current)

  cmd.run([cmd.name, '1'])
  cmdproc = dbgr.core.processor
  cmds = cmdproc.commands
  break_cmd = cmds['break']
  break_cmd.run([break_cmd.name, cmdproc.frame.source_location[0].to_s])
  cmd.run([cmd.name, '1', 'x' '>' '10'])
  cmd.run([cmd.name, '1'])
end
