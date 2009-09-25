# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::InfoFrame < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show information about the selected frame'
    MIN_ABBREV   = 'fr'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    SHORT_HELP   = HELP.split("\n")[0]
  end

  def run(args)
    frame = @proc.frame
    line_no   = if @proc.core.event == 'vm-insn'
                  frame.iseq.offset2lines[frame.pc_offset][0]
                else
                  frame.source_location[0]
                end

    msg("Line %s of %s at PC offset %d, type: %s" %
        [line_no, frame.source_container[1], frame.pc_offset, 
         frame.type])
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
