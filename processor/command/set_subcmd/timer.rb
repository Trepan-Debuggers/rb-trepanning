# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'

class Trepan::Subcommand::SetTimer < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{PREFIX.join(' ')} [on|off]

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
    EOH

    MIN_ABBREV = 'ti'.size
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
  require_relative '../../hook'

  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetTimer)
  cmd.run(cmd.prefix)
  %w(off on).each do |arg|
    cmd.run(cmd.prefix + [arg])
  end
end
