# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../running'
require_relative '../../app/breakpoint' # FIXME: possibly temporary

class Trepan::Command::ContinueCommand < Trepan::Command
  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [breakpoint-position]

Leave the debugger loop and continue execution. Subsequent entry to
the debugger however may occur via breakpoints or explicit calls, or
exceptions.

If a parameter is given, a temporary breakpoint is set at that position
before continuing. Offset are numbers prefixed with an "O" otherwise
the parameter is taken as a line number.

Examples:
   #{NAME}
   #{NAME} 10    # continue to line 10
   #{NAME} o20   # continue to VM Instruction Sequence offset 20
   #{NAME} gcd   # continue to first instruction of method gcd
   #{NAME} IRB.start o7 # continue to IRB.start offset 7

See also 'step', 'next', 'finish', and 'nexti' commands.
    HELP

    ALIASES      = %w(c cont)
    CATEGORY     = 'running'
    MAX_ARGS     = 2  # Need at most this many
    NEED_RUNNING = true
    SHORT_HELP   = 'Continue execution of the debugged program'
  end

  # This is the method that runs the command
  def run(args)
    if args.size == 1
      # Form is: "continue"
      @proc.continue
    else
      iseq, line_number, vm_offset, condition, negate = 
        @proc.breakpoint_position(@proc.cmd_argstr, false)
      return false unless iseq && vm_offset
      bp = @proc.breakpoint_offset(vm_offset, condition, negate, iseq)
      return unless bp
      @proc.continue
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  p cmd.run([cmd.name])
end
