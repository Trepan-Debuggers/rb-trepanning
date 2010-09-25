# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'disable'

# enable breakpoint command. Is like disable but the parameter
# to @proc.en_disable_breakpoint_by_number is different (set as
# ENABLE_PARM below).
class Trepan::Command::EnableCommand < Trepan::Command::DisableCommand

  # Silence already initialized constant .. warnings
  old_verbose = $VERBOSE  
  $VERBOSE    = nil
  HELP = 
      'enable [display] bpnumber [bpnumber ...]
    
Enables the breakpoints given as a space separated list of breakpoint
numbers. See also "info break" to get a list.
'

  ALIASES       = %w(en)
  NAME          = File.basename(__FILE__, '.rb')
  SHORT_HELP    = 'Enable some breakpoints'
  $VERBOSE      = old_verbose 

  def initialize(proc)
    super
    @enable_parm = true # true if enable 
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
  cmd.run([name, '1'])
end
