# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetDebugstack < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set display of complete stack including possibly setup stack from rbdbgr"
    IN_LIST     = false
    MIN_ABBREV  = 'debugs'.size
    NAME        = File.basename(__FILE__, '.rb')
  end

  def run(args)
    @hidelevels = @proc.hidelevels unless @hidelevels
    super
    @proc.hidelevels = @proc.settings[:debugstack] ? {} : @hidelevels
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::SetAutoeval.new(cmd)
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
  subcommand.summary_help(name)
end
