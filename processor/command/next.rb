# -*- coding: utf-8 -*-
require_relative 'base/cmd'

class Debugger::Command::NextCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"next[+|=|-|<|>|!|<>] [EVENT-NAME...] [count]

Step one statement ignoring steps into function calls at this level.
Sometimes this is called 'step over'.

With an integer argument, perform 'next' that many times. However if
an exception occurs at this level, or we 'return' or 'yield' or the
thread changes, we stop regardless of count.

A suffix of '+' on the command or an alias to the command forces to
move to another line, while a suffix of '-' does the opposite and
disables the requiring a move to a new line. If no suffix is given,
the debugger setting 'different' determines this behavior.

If no suffix is given, the debugger setting 'different'
determines this behavior.

Examples: 
  next        # next 1 event, *any* event 
  next 1      # same as above
  next+       # same but force stopping on a new line
  next=       # same but force stopping on a new line a new frame added
  next-       # same but force stopping on a new line a new frame added
  next 5/5+0  # same as above
  next line   # next using only line events
  next call   # next using only call call events
  next<>      # next using call return events at this level or below
"

    ALIASES      = %w(n next+ next- next< next> next<> next! n> n< n! n+ n- 
                      n<> n=)
    CATEGORY     = 'running'
    # execution_set = ['Running']
    MAX_ARGS     = 1   # Need at most this many. FIXME: will be eventually 2
    NAME         = File.basename(__FILE__, '.rb')
    NEED_RUNNING = true
    SHORT_HELP   = 'Step program without entering called functions'
  end

  # This method runs the command
  def run(args) # :nodoc
    opts = @proc.parse_next_step_suffix(args[0])
    if args.size == 1
      # Form is: "next" which means "next 1"
      step_count = 0
    else
      count_str = args[1]
      opts = {
        :msg_on_error => 
        "The 'next' command argument must eval to an integer. Got: %s" % 
        count_str,
        :min_value => 1
      }
      count = @proc.get_an_int(count_str, opts)
      return unless count
      # step 1 is core.step_count = 0 or "stop next event"
      step_count = count - 1  
    end
    @proc.next(step_count, opts)
  end
end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  [%w(n 5), %w(next 1+2), %w(n foo)].each do |c|
    dbgr.core.step_count = 0
    cmd.proc.leave_cmd_loop = false
    result = cmd.run(c)
    puts 'Run result: %s' % result
    puts 'step_count %d, leave_cmd_loop: %s' % [dbgr.core.step_count,
                                                cmd.proc.leave_cmd_loop]
  end
  [%w(n), %w(next+), %w(n-)].each do |c|
    dbgr.core.step_count = 0
    cmd.proc.leave_cmd_loop = false
    result = cmd.run(c)
    puts cmd.proc.different_pos
  end
end
