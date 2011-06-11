# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetTrace < Trepan::SubSubcommandMgr 
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = "Set tracing of various sorts.

The types of tracing include events from the trace buffer, or printing
those events.

See 'help #{PREFIX.join(' ')} *' for a list of subcommands or 'help set trace
<name>' for help on a particular trace subcommand."

    IN_LIST    = true
    MIN_ABBREV = 'tr'.size
    SHORT_HELP = 'Set tracing of various sorts.'
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  command = Trepan::SubSubcommand::SetTrace.new(dbgr.core.processor, 
                                               set_cmd)
  set_cmd.instance_variable_set('@last_args', command.class.const_get('CMD'))
  # require_relative '../../../lib/trepanning'
  # Trepan.debug
  command.run(command.class.const_get('PREFIX'))
end
