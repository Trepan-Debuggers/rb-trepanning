# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoFrame < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Show all information about the selected frame'
    MIN_ABBREV   = 'fr'.size # Note we have "info file"
    NEED_STACK   = true
  end

  def run(args)
    frame = @proc.frame
    msg("Line %s of %s at PC offset %d, type: %s" %
        [@proc.frame_line, frame.source_container[1], frame.pc_offset, 
         frame.type])
    if @proc.event == 'return'
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
  subcommand = Trepan::Subcommand::InfoFrame.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
