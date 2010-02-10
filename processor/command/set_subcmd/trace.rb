# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetTrace < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to display trace events as seen in the debugger.

When 'set trace' is set, calls to the event command processor that are
will be displayed. This is generally more than those events that one
might stop at and go into a command loop. For example the 'next',
'finish', or 'step+' (step different) commands may run several
intermediate steps before stopping.

However, sometimes full-speed running occurs such as one runs
'continue'.  So these events will not be shown in tracing. Similarly,
if the event mask is set not to trap certain events, those events will
not be shown either.

See also 'set events'.
"

    IN_LIST    = true
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
  end

  def run(args)
    super
    if @proc.settings[:trace]
      @proc.unconditional_prehooks.insert_if_new(-1, *@proc.trace_hook)
    else
      @proc.unconditional_prehooks.delete_by_name('trace')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  require_relative %w(.. .. hook)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetTrace.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)
  cmd.proc.hook_initialize(cmd.proc.commands)

  subcommand.run_show_bool
  subcommand.summary_help(name)
end
