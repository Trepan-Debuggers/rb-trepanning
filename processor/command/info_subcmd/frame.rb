# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Debugger::Subcommand::InfoFrame < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show all information about the selected frame'
    MIN_ABBREV   = 'fr'.size # Note we have "info file"
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info frame)
  end

  def run(args)
    frame = @proc.frame
    msg("Line %s of %s at PC offset %d, type: %s" %
        [@proc.frame_line, frame.source_container[1], frame.pc_offset, 
         frame.type])
    if @proc.core.event == 'return'
      msg("Return value class: #{@proc.frame.sp(1).class}")
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoFrame.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
