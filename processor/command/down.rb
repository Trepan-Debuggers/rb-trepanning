# -*- coding: utf-8 -*-
require_relative 'base_cmd'

class Debugger::Command::DownCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"d(own) [count]

Move the current frame down in the stack trace (to a newer frame). 0
is the most recent frame. If no count is given, move down 1.

See also 'up' and 'frame'.
"

    ALIASES       = %w(d)
    CATEGORY      = 'stack'
    MIN_ARGS      = 0  # Need at least this many
    MAX_ARGS      = 1  # Need at most this many
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = true
    SHORT_HELP    = 'Move frame in the direction of the caller of the last-selected frame'
  end
  
  require_relative %w(.. .. lib frame)
  include Debugger::Frame

  # Run 'down' command. 
  def run(args)

    if not @proc.top_frame
      errmsg("Program has no stack frame set.")
      return false
    end

    if args.size == 1
      # Form is: "down" which means "down 1"
      count = 1
    else
      stack_size = @proc.top_frame.stack_size
      count_str = args[1]
      name_or_id = args[1]
      opts = {
        :msg_on_error => 
        "The 'down' command argument must eval to an integer. Got: %s" % count_str,
        :min_value => -stack_size,
        :max_value => stack_size-1
      }
      count = @proc.get_an_int(count_str, opts)
      return false unless count
    end
    @proc.adjust_frame(count, false)
    return false
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  # FIXME: do more of the below setup in mock
  require_relative %w(.. mock)
  dbgr = MockDebugger.new

  cmds = dbgr.core.processor.instance_variable_get('@commands')
  name = File.basename(__FILE__, '.rb')
  cmd = cmds[name]
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current, Thread::current)
  cmd.run [name]
  cmd.run [name, '0']
  puts '=' * 40
  cmd.run [name, '1']
  cmd.run [name, '-2']
  puts '=' * 40
  def foo(cmd, name)
    cmd.proc.top_frame = cmd.proc.frame = RubyVM::ThreadFrame::current
    cmd.run([name])
    cmd.run([name, '-1'])
  end
  foo(cmd, name)
end
