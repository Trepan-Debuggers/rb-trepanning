require_relative './../command'
require_relative 'break'

class Trepan::Command::SetTempBreakpointCommand < 
    Trepan::Command::BreakCommand
  ALIASES      = []
  CATEGORY     = 'breakpoints'
  NAME         = File.basename(__FILE__, '.rb')
  HELP         = <<-HELP
Same as break, but the breakpoint is deleted when it is hit.

See also "break".
      HELP
  SHORT_HELP   = 'Set a temporary breakpoint'
  
  def run(args)
    super args, true
  end
end

