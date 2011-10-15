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
#{NAME} [display] NUM1 [NUM2 ...]

Disables the breakpoints or display given as a space separated list of
numbers. 

See also "enable" and "info break".
  HELP
    
    CATEGORY      = 'breakpoints'
    SHORT_HELP    = 'Disable some breakpoints or displays'
  }

  def initialize(proc)
    super
    @enable_parm = false # true if enable 
  end
  
  def run(args)
    if args.size == 1
      errmsg('No breakpoint or display number given.')
      return
    end
  if args[1] == 'display'
    args.shift
    first = args.shift
    args.each do |num_str|
      i = @proc.get_an_int(num_str)
      success = @proc.en_disable_display_by_number(i, @enable_parm) if i
      msg("Display %s #{@name}d." % i) if success
    end
  end
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
