# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2012, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'

class Trepan::Command::DebugCommand < Trepan::Command
  unless defined?(HELP)
    NAME          = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
**#{NAME}** *ruby-code*

Enter the debugger recursively on *ruby-code*.
    HELP

    CATEGORY      = 'data'
    MIN_ARGS      = 1
    MAX_ARG       = nil
    NEED_STACK    = false
    SHORT_HELP    = 'recursive debugging of an expression'

    EXTRA_DEBUG_SETUP_CALLS = 4
  end

  def run(args)
    th         = Thread.current
    frame      = @proc.frame  # gets messed up in recursive call
    arg_str    = args[1..-1].join(' ')
    hidelevels = @proc.hidelevels[th]

    stack_diff = RubyVM::Frame.stack_size - frame.stack_size

    # Ignore tracing in support routines:
    # FIXME remvoe 1.9.3 hack
    if '1.9.3' != RUBY_VERSION
      tf = @proc.dbgr.trace_filter
      [self.method(:run), @proc.method(:debug_eval),
       @proc.method(:debug_eval_with_exception),
       @proc.method(:get_binding_and_filename),
       @proc.method(:fake_eval_filename)].each do |m|
        tf << m unless tf.member?(m)
      end
    end

    @proc.hidelevels[th] += stack_diff + EXTRA_DEBUG_SETUP_CALLS

    # Values we need to save before munging them
    old_tracing            = th.tracing
    old_exec_event_tracing = th.exec_event_tracing?
    old_mutex              = @proc.core.mutex
    old_next_level         = @proc.next_level
    old_step_count         = @proc.core.step_count

    section 'ENTERING NESTED DEBUGGER'

    # Things we need to do to allow entering the debugger again
    @proc.debug_nest      += 1
    @proc.core.mutex       = Mutex.new
    th.tracing             = false
    th.exec_event_tracing  = false
    @proc.next_level       = 32000

    RubyVM::Frame.current.trace_off = false
    @proc.core.step_count  = 0
    retval = @proc.debug_eval(arg_str, 15,
                              RUBY_VERSION == '1.9.3') # FIXME

    # Restore munged values
    th.exec_event_tracing  = old_exec_event_tracing
    th.tracing             = old_tracing
    @proc.core.mutex       = old_mutex
    @proc.frame_setup(frame)
    @proc.hidelevels[th]   = hidelevels
    @proc.core.step_count  = old_step_count
    @proc.next_level       = old_next_level
    @proc.print_location
    @proc.debug_nest      -= 1
    section 'LEAVING NESTED DEBUGGER'
    msg "R=> #{retval.inspect}"
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.proc.hidelevels[Thread.current] = 0
  cmd.proc.frame_setup(RubyVM::Frame.get)
  cmd.run([cmd.name, 'x = 1; y = 2'])
end
