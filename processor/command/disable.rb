# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. breakpoint)
require_relative %w(.. .. app brkpt)
class Debugger::Command::DisableCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
      'disable [display] bpnumber [bpnumber ...]
    
Disables the breakpoints given as a space separated list of breakpoint
numbers. See also "info break" to get a list.
'

    CATEGORY      = 'breakpoints'
    NAME          = File.basename(__FILE__, '.rb')
    SHORT_HELP    = 'Disable some breakpoints'
  end
  
  def run(args)
    if args.size == 1
      errmsg('No breakpoint number given.')
      return
    end
#   if args[1] == 'display'
#     display_enable(args[2:], 0)
#   end
    first = args.shift
    args.each do |num_str|
      i = @proc.get_an_int(num_str)
      success = @proc.en_disable_breakpoint_by_number(num_str.to_i, false) if i
      msg('Breakpoint %s disabled.' % i) if success
    end
  end
end
        
if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
  cmd.run([name, '1'])
  cmdproc = dbgr.core.processor
  cmds = dbgr.core.processor.commands
  break_cmd = cmds['break']
  break_cmd.run(['break', cmdproc.frame.source_location[0].to_s])
  # require_relative %w(.. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  cmd.run([name, '1'])
end
