# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::InfoProgram < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Information about debugged program and its environment'
    MIN_ABBREV   = 'pr'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info program)
  end

  def run(args)
    frame = @proc.frame
    m = 'Program stop event: %s' % @proc.core.event
    m += 
      if frame.iseq
        '; PC offset %d of instruction sequence: %s' % 
          [frame.pc_offset, frame.iseq.name]
      else
        '.'
      end
    msg m
    msg '=> %s' % @proc.frame.sp(1).inspect if 
      'return' == @proc.core.event 

    if @proc.brkpt
      msg('It is stopped at %sbreakpoint %d.' %
          [@proc.brkpt.temp? ? 'temporary ' : '',
           @proc.brkpt.id])
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoProgram.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
