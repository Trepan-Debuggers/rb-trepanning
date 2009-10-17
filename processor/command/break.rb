require_relative %w(base cmd)
require_relative %w(.. running)
require_relative %w(.. .. lib brkpt) # FIXME: possibly temporary
class Debugger::Command::BreakCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
'break [offset|line number]

Leave the debugger loop and break execution. Subsequent entry to
the debugger however may occur via breakpoints or explicit calls, or
exceptions.

If a parameter is given, a temporary breakpoint is set at that position
before continuing. Offset are numbers preficed with an "O" otherwise
the parameter is taken as a line number.

Examples:
   break
   break 10    # set breakpoint on line 10
   break o20   # set breakpoint VM Instruction Sequence offset 20
'

    ALIASES      = %w(c)
    CATEGORY     = 'running'
    NAME         = File.basename(__FILE__, '.rb')
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Set a breakpoint'
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size == 1
      # Form is: "break" FIXME: Run info break? 
      # ???
    else
      # FIXME: handle more general condition parameter rather than just
      # a line number
      line_number_str = args[1]
      if line_number_str.size > 0 && line_number_str[0].downcase == 'o'
        use_offset = true
        line_number_str[0] = ''
      else
        use_offset = false
      end
      opts = {
        :msg_on_error => 
        "The 'break' command argument must eval to an integer. Got: %s" % line_number_str,
        :min_value => 0
      }
      line_number = @proc.get_an_int(line_number_str, opts)
      return false unless line_number
      @proc.breakpoint(line_number, use_offset) # should handle condition
    end
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
