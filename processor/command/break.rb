require_relative %w(base cmd)
require_relative %w(.. breakpoint)
require_relative %w(.. .. lib brkpt)
class Debugger::Command::BreakCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
'break [line number|offset]

With a line number argument, set a break there in the current
instruction sequence.  With an offset (a number prefaced with an "O")
set a breakpoint at that instruction offset.

Examples:
   break
   break 10    # set breakpoint on line 10
   break o20   # set breakpoint VM Instruction Sequence offset 20
'

    ALIASES      = %w(b)
    CATEGORY     = 'breakpoints'
    NAME         = File.basename(__FILE__, '.rb')
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Set a breakpoint'
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size == 1
      use_offset = true
      position = @proc.frame.pc_offset
    else
      # FIXME: handle more general condition parameter rather than just
      # a line number
      position_str = args[1]
      if position_str.size > 0 && position_str[0].downcase == 'o'
        use_offset = true
        position_str[0] = ''
      else
        use_offset = false
      end
      opts = {
        :msg_on_error => 
        "The 'break' command argument must eval to an integer. Got: %s" % position_str,
        :min_value => 0
      }
      position = @proc.get_an_int(position_str, opts)
      return false unless position
    end
    bp = @proc.breakpoint(position, use_offset) # should handle condition
    if bp
      msg("Breakpoint %d set in %s,\n\tVM offset %d of instruction sequence %s." %
          [bp.id, bp.iseq.source_container.join(' '),
           bp.offset, bp.iseq.name] )
    end
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end
