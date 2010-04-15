# -*- coding: utf-8 -*-
require_relative '../base/subsubcmd'

class Debugger::Subcommand::SetTimer < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "set timer [on|off|0|1]

Tracks and shows elapsed time between debugger events.

Since debugger overhead can be large depending on what you are doing,
there are many ways to customize the debugger to take less time (and
do less).

Stepping is slow, running to a breakpoint without stepping is
relatively fast compared to previous versions of the debugger and
compared to stepping. 

Stopping at fewer events can also speed things up. Trace event
buffering slows things down.

Buy turning this setting on, you may be able to get a feel for what
how expensive the various settings.

See also: 'set events', 'set trace buffer', 'step', and 'break'.
"

    MIN_ABBREV = 'ti'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set timer)
    SHORT_HELP = "Set to show elapsed time between debugger events"
  end

  def run(args)
    super
    if @proc.settings[:timer]
      @proc.cmdloop_posthooks.insert_if_new(-1, 'timer', @proc.timer_hook[1])
      @proc.cmdloop_prehooks.insert_if_new(-1, 'timer', @proc.timer_hook[1])
    else
      @proc.cmdloop_posthooks.delete_by_name('timer')
      @proc.cmdloop_prehooks.delete_by_name('timer')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  require_relative '../../hook'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  subcommand    = Debugger::Subcommand::SetTimer.new(set_cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run_show_bool
  subcommand.summary_help(name)

  # require 'rbdbgr'
  # Debugger.debug(:set_restart => true)
  subcommand.run(['set', name])
  subcommand.run(['set', name, 'off'])
  subcommand.run(['set', name, 'on'])

end
