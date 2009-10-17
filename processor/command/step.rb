# -*- coding: utf-8 -*-
require_relative %w(base cmd)
class Debugger::Command::StepCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"step[+|-|<|>|!|<>] [EVENT-NAME...] [count]
Execute the current line, stopping at the next event.

With an integer argument, step that many times.

EVENT-NAME... is list of an event name which is one on 'call',
'return', 'line', 'exception' 'c-call', 'c-return' or 'c-exception'.
If specified, only those stepping events will be considered. If no
list of event names is given, then any event triggers a stop when the
count is 0.

There is however another way to specify an *single* EVENT-NAME, by
suffixing one of the symbols '<', '>', or '!' after the command or on
an alias of that.  A suffix of '+' on a command or an alias forces a
move to another position, while a suffix of '-' disables this requirement.
A suffix of '>' will continue until the next call. ('finish' will run
run until the return for that call.)

If no suffix is given, the debugger setting 'different'
determines this behavior.

Examples: 
  step        # step 1 event, *any* event 
  step 1      # same as above
  step 5/5+0  # same as above
  step line   # step only line events
  step call   # step only call call events
  step>       # step call and C-call events
  step<       # step only return and C-return events
  step call line   # Step line *and* call events
  step<>      # same as step call c-call return c-return 


Related and similar is the 'next' command.  See also the commands:
'skip', 'jump' (there's no 'hop' yet), 'continue', 'return' and
'finish' for other ways to progress execution."

    ALIASES      = %w(s step+ step- step< step> step<> step! s> s< s+ s- s<> s!)
    CATEGORY     = 'running'
    MAX_ARGS     = 1   # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    SHORT_HELP   = 'Step program (possibly entering called functions)'
  end

  # This method runs the command
  def run(args) # :nodoc
    opts = @proc.parse_next_step_suffix(args[0])
    if args.size == 1
      # Form is: "step" which means "step 1"
      step_count = 0
    else
      count_str = args[1]
      opts = {
        :msg_on_error => 
        "The 'step' command argument must eval to an integer. Got: %s" % 
        count_str,
        :min_value => 1
      }.merge(opts)
      count = @proc.get_an_int(count_str, opts)
      return unless count
      # step 1 is core.step_count = 0 or "stop next event"
      step_count = count - 1  
    end
    @proc.step(step_count, opts)
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
