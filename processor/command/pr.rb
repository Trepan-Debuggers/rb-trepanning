# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../eval'
class Trepan::Command::PrCommand < Trepan::Command

  unless defined?(HELP)
    NAME          = File.basename(__FILE__, '.rb')
    HELP = 
      "#{NAME} EXPRESSION

Print the value of the EXPRESSION. Variables accessible are those of the
environment of the selected stack frame, plus globals. 

If the length output string large, the first part of the value is
shown and ... indicates it has been truncated.

See 'set max string' to change the string truncation limit.
"

    # ALIASES       = %w(p)
    CATEGORY      = 'data'
    SHORT_HELP    = 'print expression truncating long output'
  end
  
  def run(args)
    msg @proc.debug_eval(@proc.cmd_argstr, @proc.settings[:maxstring])
  end
end
        
if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  arg_str = '1 + 2'
  cmd.proc.instance_variable_set('@cmd_argstr', arg_str)
  cmd.run([name, arg_str])
  cmdproc = dbgr.core.processor
  cmds = dbgr.core.processor.commands
end
