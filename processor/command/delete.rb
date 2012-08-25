# -*- coding: utf-8 -*-
# Copyright (C) 2010-2012 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../breakpoint'
require_relative '../../app/breakpoint'
class Trepan::Command::DeleteCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [bpnumber [bpnumber...]]  

Delete some breakpoints.

Arguments are breakpoint numbers with spaces in between.  To delete
all breakpoints, give no argument.  those breakpoints.  Without
argument, clear all breaks (but first ask confirmation).
    
See also the "clear" command which clears breakpoints by line/file
number.
    HELP

    CATEGORY      = 'breakpoints'
    SHORT_HELP    = 'Delete some breakpoints'
    ALIASES       = %w(d)
  end
  
  def run(args)
    if args.size == 1
      if confirm('Delete all breakpoints?', false)
        @proc.brkpts.reset
        return
      end
    end
    first = args.shift
    args.each do |num_str|
      opts = {:msg_on_error => '%s must be a number' % num_str}
      i = @proc.get_an_int(num_str, opts)
      if i 
        success = @proc.delete_breakpoint_by_number(num_str.to_i, false) if i
        msg('Deleted breakpoint %d.' % i) if success
      end
    end
  end
end
        
if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.run([cmd.name])
  cmd.run([cmd.name, '1'])
  cmdproc = dbgr.core.processor
  cmds = dbgr.core.processor.commands
  break_cmd = cmds[cmd.name]
  break_cmd.run([cmd.name, cmdproc.frame.source_location[0].to_s])
  # require_relative '../../lib/trepanning'
  # Trepan.debug
  cmd.run([cmd.name, '1'])
end
