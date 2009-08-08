# -*- coding: utf-8 -*-
require_relative File.join(%w(.. base_subcmd))

class Debugger::Subcommand::SetWidth < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = "Set number of characters the debugger thinks are in a line"
    IN_LIST      = true
    MIN_ABBREV   = 'wid'.size
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = HELP
  end

  def run(args)
    run_set_int(args.join(' '),
                "The 'width' command requires a line width", 
                0, nil)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative File.join(%w(.. .. mock))
  require_relative File.join(%w(.. .. subcmd))
  dbgr = MockDebugger.new
  cmds = dbgr.core.processor.instance_variable_get('@commands')
  cmd = cmds['exit']
  subcommand = Debugger::Subcommand::SetWidth.new(cmd)
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
  subcommand.run_show_int
  subcommand.summary_help('width')
end
