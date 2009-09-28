# -*- coding: utf-8 -*-
require_relative 'base_cmd'
# Mstack     = import_relative('stack', '...lib')

class Debugger::Command::FinishCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"finish [levels]

Continue execution until leaving the current function. When `level' is
specified, that many frame levels need to be popped. The default is 1.
Note that 'yield' and exceptions raised my reduce the number of stack
frames. Also, if a thread is switched, we stop ignoring levels.

next> is similar in that it stops at a return, but it doesn't
guarentee the stack level is the same as or less than the current
one. 

See the break command if you want to stop at a particular point in a
program. In general, finish, step and next may slow a program down
while 'break' will have less overhead."

    ALIASES      = %w(fin)
    CATEGORY     = 'running'
    # execution_set = ['Running']
    MAX_ARGS     = 1   # Need at most this many. 
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    SHORT_HELP   = 'Step program without entering called functions'
  end

  # This method runs the command
  def run(args) # :nodoc
    opts = {}
    if args.size == 1
      # Form is: "finish" which means "finish 1"
      level_count = 0
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
      # step 1 is core.level_count = 0 or "stop next event"
      level_count = count - 1  
    end
    @proc.finish(level_count, opts)
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  [%w(finish 1), %w(fin 2-1), %w(n foo)].each do |c|
    cmd.proc.next_level = 0
    cmd.proc.leave_cmd_loop = false
    result = cmd.run(c)
    puts 'Run result: %s' % result
    puts 'level_count %d, leave_cmd_loop: %s' % [cmd.proc.next_level,
                                                cmd.proc.leave_cmd_loop]
  end
  [%w(fin), %w(finish)].each do |c|
    cmd.proc.next_level = 0
    cmd.proc.leave_cmd_loop = false
    result = cmd.run(c)
    puts cmd.proc.different_pos
  end
end
