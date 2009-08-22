require_relative 'base_cmd'
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
    MIN_ARGS     = 0   # Need at least this many
    MAX_ARGS     = 1   # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Continue execution of debugged program'
  end

  # This method runs the command
  def run(args) # :nodoc
    @proc.core.step_count = -1    # No more event stepping
    @proc.leave_cmd_loop  = true  # Break out of the processor command loop.
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
