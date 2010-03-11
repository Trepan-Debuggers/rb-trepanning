# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetTrace < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "set trace [on|off]
set trace var GLOBAL_VARIABLE

In the first form to display trace events as seen in the debugger.  

When 'set trace' is set, calls to the event command processor that are
will be displayed. This is generally more than those events that one
might stop at and go into a command loop. For example the 'next',
'finish', or 'step+' (step different) commands may run several
intermediate steps before stopping.

However, sometimes full-speed running occurs such as one runs
'continue'.  So these events will not be shown in tracing. Similarly,
if the event mask is set not to trap certain events, those events will
not be shown either.

In the second form, the debugger calls 'trace_var' to trace changes to
the value of global variable.  Note in contrast to other events
stopping for variable tracing occurs *after* the event, not before.

See also 'set events'.
"

    IN_LIST    = true
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
    SHORT_HELP = "Set to display trace events or trace a global variable."
  end

  def run(args)
    if args.size == 4
      if args[2] == 'var'
        traced_var = args[3]
        unless traced_var[0] == '$'
          errmsg "Expecting a global variable to trace, got: #{traced_var}"
          return
        end
        trace_var(traced_var, @proc.core.method(:trace_var_processor))
        msg("Tracing variable #{traced_var}.")
        return
      end
    end
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
