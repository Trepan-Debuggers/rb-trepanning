# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'

class Trepan::Command::NextCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME}[+|=|-|<|>|!|<>] [EVENT-NAME...] [count]

Step one statement ignoring steps into function calls at this level.
Sometimes this is called 'step over'.

With an integer argument, perform '#{NAME}' that many times. However if
an exception occurs at this level, or we 'return' or 'yield' or the
thread changes, we stop regardless of count.

A suffix of '+' on the command or an alias to the command forces to
move to another line, while a suffix of '-' does the opposite and
disables the requiring a move to a new line. If no suffix is given,
the debugger setting 'different' determines this behavior.

If no suffix is given, the debugger setting 'different'
determines this behavior.

Examples: 
  #{NAME}        # #{NAME} 1 event, *any* event 
  #{NAME} 1      # same as above
  #{NAME}+       # same but force stopping on a new line
  #{NAME}=       # same but force stopping on a new line a new frame added
  #{NAME}-       # same but force stopping on a new line a new frame added
  #{NAME} 5/5+0  # same as above
  #{NAME} line   # #{NAME} using only line events
  #{NAME} call   # #{NAME} using only call call events
  #{NAME}<>      # #{NAME} using call return events at this level or below
    HELP

    ALIASES      = %W(n #{NAME}+ #{NAME}- #{NAME}< #{NAME}> #{NAME}<> #{NAME}! n> n< n! n+ n- 
                      n<> n=)
    CATEGORY     = 'running'
    # execution_set = ['Running']
    MAX_ARGS     = 1   # Need at most this many. FIXME: will be eventually 2
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
        "The '#{NAME}' command argument must eval to an integer. Got: %s" % 
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
  dbgr, cmd = MockDebugger::setup
  [%w(n 5), %W(#{cmd.name} 1+2), %w(n foo)].each do |c|
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
