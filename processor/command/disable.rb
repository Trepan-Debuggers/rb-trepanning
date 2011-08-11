# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../breakpoint'
require_relative '../../app/breakpoint'
require_relative '../../app/util'

# disable breakpoint command. The difference however is that the
# parameter to @proc.en_disable_breakpoint_by_number is different (set
# as ENABLE_PARM below).
#
# NOTE: The enable command  subclasses this, so beware when changing! 
class Trepan::Command::DisableCommand < Trepan::Command

  Trepan::Util.suppress_warnings {
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [display] bpnumber [bpnumber ...]
    
Disables the breakpoints given as a space separated list of breakpoint
numbers. See also "info break" to get a list.
  HELP
    
    CATEGORY      = 'breakpoints'
    SHORT_HELP    = 'Disable some breakpoints'
  }

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
  dbgr, cmd = MockDebugger::setup
  cmd.run([cmd.name])
  cmd.run([cmd.name, '1'])
  cmdproc = dbgr.core.processor
  cmds = cmdproc.commands
  break_cmd = cmds['break']
  break_cmd.run(['break', cmdproc.frame.source_location[0].to_s])
  # require_relative '../../lib/trepanning'
  # Trepan.debug
  cmd.run([cmd.name, '1'])
end
