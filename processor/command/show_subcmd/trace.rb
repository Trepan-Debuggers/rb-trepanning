# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::ShowTrace < Debugger::SubSubcommandMgr 

  unless defined?(HELP)
    HELP = "Show event tracing"
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show trace)
    SHORT_HELP = HELP
  end

  def run(args)
    if args.size == 2
      run_show_bool(SHORT_HELP)
    else
      super
    end

    # FIXME: DRY This with ShowBoolSubcommand
    # Generic subcommand showing a boolean-valued debugger setting.
    def run_show_bool(what=nil)
      val = show_onoff(@proc.settings[@name])
      what = @name unless what
      @proc.msg("%s is %s." % [what, val])
    end

  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  show_cmd = cmds['show']
  command = Debugger::SubSubcommand::ShowTrace.new(dbgr.core.processor, 
                                                   show_cmd)

  name = File.basename(__FILE__, '.rb')
  cmd_args = ['show', name]
  command.run(cmd_args)
end
