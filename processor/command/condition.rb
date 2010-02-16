# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. breakpoint)
require_relative %w(.. .. app brkpt)
require_relative %w(.. .. app condition)

class Debugger::Command::ConditionCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
'condition BP_NUMBER CONDITION

BP_NUMBER is a breakpoint number.  CONDITION is an expression which
must evaluate to True before the breakpoint is honored.  If CONDITION
is absent, any existing condition is removed; i.e., the breakpoint is
made unconditional.

Examples:
   condition 5 x > 10  # Breakpoint 5 now has condition x > 10
   condition 5         # Remove above condition
'

    ALIASES       = %w(cond)
    CATEGORY      = 'breakpoints'
    MIN_ARGS      = 1
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Specify breakpoint number N to break only if COND is true'
  end

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
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
  
  cmd.run([name, '1'])
  cmdproc = dbgr.core.processor
  cmds = cmdproc.commands
  break_cmd = cmds['break']
  break_cmd.run(['break', cmdproc.frame.source_location[0].to_s])
  cmd.run([name, '1', 'x' '>' '10'])
  cmd.run([name, '1'])
end
