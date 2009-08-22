# -*- coding: utf-8 -*-
require_relative 'base_cmd'
class Debugger::Command::WhereCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"where [count]

Print a stack trace, with the most recent frame at the top.  With a
positive number, print at most many entries.  With a negative number
print the top entries minus that number.

An arrow indicates the 'current frame'. The current frame determines
the context used for many debugger commands such as expression
evaluation or source-line listing.

Examples:
   where    # Print a full stack trace
   where 2  # Print only the top two entries
   where -1 # Print a stack trace except the initial (least recent) call."

    ALIASES       = %w(bt backtrace)
    CATEGORY      = 'stack'
    MIN_ARGS      = 0  # Need at least this many
    MAX_ARGS      = 1  # Need at most this many
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = true
    SHORT_HELP    = 'Print backtrace of stack frames'
  end

  require_relative %w(.. .. lib frame)
  include Debugger::Frame

  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      stack_size = @proc.top_frame.stack_size
      count = @proc.get_int(args[1], 
                            :cmdname   => 'where',
                            :max_value => stack_size-1)
      return false unless count
    end
    if @proc.frame
      print_stack_trace(@proc.top_frame, count, @proc.frame_index)
    else
      errmsg 'No frame.'
    end
    return false  # Don't break out of cmd loop
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  cmd.proc.frame_setup(RubyVM::ThreadFrame::current, Thread::current)

  def sep ; puts '=' * 40 end
  cmd.run [name]
  sep
  %w(1 100).each {|count| cmd.run(count); sep }
  def foo(cmd, name)
    cmd.proc.top_frame = cmd.proc.frame = RubyVM::ThreadFrame::current
    cmd.run([name])
  end
  foo(cmd, name)
end
