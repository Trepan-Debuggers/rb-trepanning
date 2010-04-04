require_relative 'base/cmd'
require_relative '../running'
require_relative '../../app/brkpt' # FIXME: possibly temporary
class Debugger::Command::ContinueCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
'continue [offset|line number]

Leave the debugger loop and continue execution. Subsequent entry to
the debugger however may occur via breakpoints or explicit calls, or
exceptions.

If a parameter is given, a temporary breakpoint is set at that position
before continuing. Offset are numbers preficed with an "O" otherwise
the parameter is taken as a line number.

Examples:
   continue
   continue 10    # continue to line 10
   continue o20   # continue to VM Instruction Sequence offset 20
'

    ALIASES      = %w(c)
    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    NEED_RUNNING = true
    SHORT_HELP   = 'Continue execution of debugged program'
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size == 1
      # Form is: "continue"
      @proc.continue
    else
      # FIXME: handle more general condition parameter rather than just
      # a line number
      position, iseq, use_offset, condition = 
        @proc.breakpoint_position(args[1..-1])
      return false unless position && iseq
      bp = 
        if use_offset
          @proc.breakpoint_offset(position, iseq, true)
        else
          @proc.breakpoint_line(position, iseq, true)
        end
      return unless bp
      @proc.continue
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
