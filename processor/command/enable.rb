# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. breakpoint)
require_relative %w(.. .. lib brkpt)
class Debugger::Command::EnableCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
      'enable [display] bpnumber [bpnumber ...]
    
Enables the breakpoints given as a space separated list of breakpoint
numbers. See also "info break" to get a list.
'

    ALIASES       = %w(en)
    CATEGORY      = 'breakpoints'
    NAME          = File.basename(__FILE__, '.rb')
    SHORT_HELP    = 'Enable some breakpoints'
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
      success = @proc.en_disable_breakpoint_by_number(num_str.to_i, true) if i
      msg('Breakpoint %s enabled.' % i) if success
    end
  end
end
        

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  p cmd.run([name])
end