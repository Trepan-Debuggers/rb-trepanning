# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::InfoFrame < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show information about the selected frame'
    MIN_ABBREV   = 'fr'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
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
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::InfoFrame.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
