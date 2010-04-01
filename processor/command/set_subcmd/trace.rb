# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::SetTrace < Debugger::SubSubcommandMgr 
  unless defined?(HELP)
    HELP = "Set tracing of various sorts.

The types of tracing include global variables, events from the trace buffer, or printing those events."

    IN_LIST    = true
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX = %w(set trace)
    SHORT_HELP = 'Set tracing of various sorts.'
  end

  def run(args)
    if args.size == 4
      super
      return
    elsif args.size == 3
      super
      return
    end
    run_set_bool(args)
    if @proc.settings[:trace]
      @proc.unconditional_prehooks.insert_if_new(-1, *@proc.trace_hook)
    else
      @proc.unconditional_prehooks.delete_by_name('trace')
    end
  end

  # FIXME: DRY code by putting the below and that from SetBoolSubcommand
  # in a mixin module.
  def run_set_bool(args, default=true)
    onoff_arg = args.size < 3 ? 'on' : args[2]
    begin
      settings[@name] = @proc.get_onoff(onoff_arg)
      run_show_bool
    rescue NameError, TypeError
    end
  end

  # Generic subcommand showing a boolean-valued debugger setting.
  def run_show_bool(what=nil)
    val = show_onoff(@proc.settings[@name])
    what = @name unless what
    @proc.msg("%s is %s." % [what, val])
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  require_relative %w(.. .. hook)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  command = Debugger::SubSubcommand::SetTrace.new(dbgr.core.processor,
                                                  set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative %w(.. .. .. lib rbdbgr)
  # Debugger.debug(:set_restart => true)
  command.run(cmd_args)
  command.run(['set', name, '*'])
end
