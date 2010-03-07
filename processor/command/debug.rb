# -*- coding: utf-8 -*-
require 'thread_frame'
require_relative %w(base cmd)

class Debugger::Command::DebugCommand < Debugger::Command
  unless defined?(HELP)
    HELP =
"debug RUBY-CODE

Enter the debugger recursively on RUBY-CODE."

    CATEGORY      = 'data'
    MIN_ARGS      = 1
    MAX_ARG       = nil
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'recursive debugging of an expression'
  end

  def run(args)
    th          = Thread.current
    frame       = @proc.frame  # gets messed up in recursive call
    arg_str     = args[1..-1].join(' ')
    hidelevels  = @proc.hidelevels[th]

    # FIXME save/restore hidelevels so "where" doesn't show debugger
    stack_diff = RubyVM::ThreadFrame.current.stack_size - frame.stack_size 
    @proc.hidelevels[th] += stack_diff + 1

    # Ignore tracing in support routines:
    # this method and debug_eval.
    tf = @proc.core.dbgr.trace_filter 
    me = self.method(:run)
    eval_me = @proc.method(:debug_eval)
    # FIXME if eval_me is added to ignore
    # we skip over things eval calls.
    [me, eval_me].each do |m|
      tf << m unless tf.member?(m)
    end

    old_tracing            = th.tracing
    old_exec_event_tracing = th.exec_event_tracing
    old_step_count         = @proc.core.step_count
    old_next_level         = @proc.next_level

    msg 'ENTERING RECURSIVE DEBUGGER'
    th.tracing             = false
    th.exec_event_tracing  = false
    @proc.core.step_count  = 0
    @proc.next_level       = 32000
    @proc.debug_eval(arg_str)
    th.exec_event_tracing  = old_exec_event_tracing
    th.tracing             = old_tracing
    msg 'LEAVING RECURSIVE DEBUGGER'
    @proc.frame_setup(frame)
    @proc.hidelevels[th]   = hidelevels
    @proc.core.step_count  = old_step_count
    @proc.next_level       = old_next_level
    @proc.print_location
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  name = File.basename(__FILE__, '.rb')
  cmd.proc.hidelevels[Thread.current] = 0
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
  cmd.run([name, 'x = 1; y = 2'])
end
