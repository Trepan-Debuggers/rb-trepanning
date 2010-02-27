# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. .. app condition)
class Debugger::Command::StepCommand < Debugger::Command

  unless defined?(HELP)
    HELP =
"step[+|=|-|<|>|!|<>] [EVENT-NAME...] [count]
step until EXPRESSION
step to METHOD-NAME

Execute the current line, stopping at the next event.  Sometimes this
is called 'step into'.

With an integer argument, step that many times.  With an 'until'
expression that expression is evaluated and we stop the first time it
is true.

EVENT-NAME... is list of an event name which is one on 'call',
'return', 'line', 'exception' 'c-call', 'c-return' or 'c-exception'.
If specified, only those stepping events will be considered. If no
list of event names is given, then any event triggers a stop when the
count is 0.

There is however another way to specify an *single* EVENT-NAME, by
suffixing one of the symbols '<', '>', or '!' after the command or on
an alias of that.  A suffix of '+' on a command or an alias forces a
move to another position, while a suffix of '-' disables this
requirement.  A suffix of '>' will continue until the next
call. ('finish' will run run until the return for that call.)

If no suffix is given, the debugger setting 'different' determines
this behavior.

Examples: 
  step        # step 1 event, *any* event obeying 'set different' setting
  step 1      # same as above
  step+       # same but force stopping on a new line
  step=       # same but force stopping on a new line a new frame added
  step-       # same but force stopping on a new line a new frame added
  step 5/5+0  # same as above
  step line   # step only line events
  step call   # step only call call events
  step>       # step call and C-call events
  step<       # step only return and C-return events
  step call line   # Step line *and* call events
  step<>      # same as step call c-call return c-return 

  step until a > b

Related and similar is the 'next' command.  See also the commands:
'skip', 'jump' (there's no 'hop' yet), 'continue', 'return' and
'finish' for other ways to progress execution.
"

    ALIASES      = %w(s step+ step- step< step> step<> step! s> s< s+ s- 
                      s<> s! s=)
    CATEGORY     = 'running'
    NAME         = File.basename(__FILE__, '.rb')
    NEED_RUNNING = true
    SHORT_HELP   = 'Step program (possibly entering called functions)'
  end

  # This method runs the command
  def run(args) # :nodoc
    opts = @proc.parse_next_step_suffix(args[0])
    condition = nil
    if args.size == 1
      # Form is: "step" which means "step 1"
      step_count = 0
    else
      if 'until' == args[1]
        try_condition = args[2..-1].join(' ')
        if valid_condition?(try_condition)
          condition = try_condition
          opts[:different_pos] = false
          step_count = 0
        end
      elsif 'to' == args[1]
        if args.size != 3
          errmsg('Expecting a method name after "to"')
          return
        elsif !@proc.method?(args[2])
          errmsg("#{args[2]} doesn't seem to be a method name")
          return
        else
          opts[:to_method] = args[2]
          opts[:different_pos] = false
          step_count = 0
        end
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
    end
    @proc.step(step_count, opts, condition)
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
