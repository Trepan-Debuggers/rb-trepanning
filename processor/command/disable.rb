# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../breakpoint'
require_relative '../../app/brkpt'

# disable breakpoint command. The difference however is that the
# parameter to @proc.en_disable_breakpoint_by_number is different (set
# as ENABLE_PARM below).
#
# NOTE: The enable command  subclasses this, so beware when changing! 
class Trepan::Command::DisableCommand < Trepan::Command

  # Silence already initialized constant .. warnings
  old_verbose = $VERBOSE  
  $VERBOSE    = nil
  HELP = 
      'disable [display] bpnumber [bpnumber ...]
    
Disables the breakpoints given as a space separated list of breakpoint
numbers. See also "info break" to get a list.
'

  CATEGORY      = 'breakpoints'
  NAME          = File.basename(__FILE__, '.rb')
  SHORT_HELP    = 'Disable some breakpoints'

  $VERBOSE      = old_verbose 

  def initialize(proc)
    super
    @enable_parm = false # true if enable 
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
      success = @proc.en_disable_breakpoint_by_number(i, @enable_parm) if i
      msg("Breakpoint %s #{@name}d." % i) if success
    end
  end
end
        
if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
  cmd.run([name, '1'])
  cmdproc = dbgr.core.processor
  cmds = cmdproc.commands
  break_cmd = cmds['break']
  break_cmd.run(['break', cmdproc.frame.source_location[0].to_s])
  # require_relative '../../lib/trepanning'
  # Trepan.debug(:set_restart => true)
  cmd.run([name, '1'])
end
