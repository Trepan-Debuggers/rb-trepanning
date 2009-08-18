require_relative 'base_cmd'
class Debugger::Command::ContinueCommand < Debugger::Command

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
  SHORT_HELP  = 'Exit program via "exit!"'

  # This method runs the command
  def run(args) # :nodoc
    return true
  end
end

if __FILE__ == $0
  name = File.basename(__FILE__, '.rb')
  cmd = Debugger::Command::ContinueCommand.new(nil)
  def cmd.msg(message)
    puts message
  end
  p cmd.run([name])
end
