require_relative %w(base cmd)
require_relative %w(.. running)
require_relative %w(.. .. lib brkpt) # FIXME: possibly temporary
class Debugger::Command::ContinueCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
'continue [[file:]lineno | function]

Leave the debugger loop and continue execution. Subsequent entry to
the debugger however may occur via breakpoints or explicit calls, or
exceptions.

If a line position is given, a temporary breakpoint is set at that
position before continuing.'

    ALIASES      = %w(c)
    CATEGORY     = 'running'
    NAME         = File.basename(__FILE__, '.rb')
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Continue execution of debugged program'
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size == 1
      # Form is: "continue"
      @proc.continue
    else
      # FIXME: handle more general condition parameter rather than just
      # a line number
      line_number_str = args[1]
      opts = {
        :msg_on_error => 
        "The 'continue' command argument must eval to an integer. Got: %s" % line_number_str,
        :min_value => 0
      }
      line_number = @proc.get_an_int(line_number_str, opts)
      return false unless line_number
      @proc.continue(line_number) # should handle condition
    end
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
