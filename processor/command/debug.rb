# -*- coding: utf-8 -*-
require_relative %w(base cmd)

class Debugger::Command::DebugCommand < Debugger::Command
  unless defined?(HELP)
    HELP =
"debug EXPRESSION

Enter the debugger recursively on EXPRESSION."

    CATEGORY      = 'data'
    MIN_ARGS      = 1
    MAX_ARG       = nil
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'recursive debugging of an expression'
  end

  def run(args)
    old_tracing = Thread.current.tracing
    arg_str = args[1..-1].join(' ')

    # Ignore tracing in support routines:
    # this method and debug_eval.
    tf = @proc.core.dbgr.trace_filter 
    me = self.method(:run)
    eval_me = @proc.method(:debug_eval)
    # FIXME if eval_me is added to ignore
    # we skip over things eval calls.
    [me].each do |m|
      tf << m unless tf.member?(m)
    end

    msg 'ENTERING RECURSIVE DEBUGGER'
    Thread.current.tracing = false
    @proc.debug_eval(arg_str)
    Thread.current.tracing = old_tracing
    msg 'LEAVING RECURSIVE DEBUGGER'
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  name = File.basename(__FILE__, '.rb')
  cmd.run([name, 'x = 1; y = 2'])
end
