# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'disable'

# enable breakpoint command. Is like disable but the parameter
# to @proc.en_disable_breakpoint_by_number is different (set as
# ENABLE_PARM below).
class Trepan::Command::EnableCommand < Trepan::Command::DisableCommand

  Trepan::Util.suppress_warnings {
    NAME        = File.basename(__FILE__, '.rb')
    HELP        = <<-HELP
#{NAME} [display] NUM1 [NUM2 ...]
    
Enables breakpoints or display NUM1. Breakpoints numbers are given as a space-
separated list numbers. 

See also "disable" and "info break".
  HELP

    ALIASES       = %w(en)
    SHORT_HELP    = 'Enable some breakpoints'
  }

  def initialize(proc)
    super
    @enable_parm = true # true if enable 
  end

end
        
if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.run([cmd.name])
  cmd.run([cmd.name, '1'])
  cmdproc = cmd.proc
  cmds = cmdproc.commands
  break_cmd = cmds['break']
  break_cmd.run(['break', cmdproc.frame.source_location[0].to_s])
  cmd.run([cmd.name, '1'])
end
