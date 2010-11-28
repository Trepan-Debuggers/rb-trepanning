# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetTrace < Trepan::SubSubcommandMgr 
  unless defined?(HELP)
    HELP = "Set tracing of various sorts.

The types of tracing include global variables, events from the trace
buffer, or printing those events.

See 'help set trace *' or a list of subcommands or 'help set trace
<name>' for help on a particular trace subcommand."

    IN_LIST    = true
    MIN_ABBREV = 'tr'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set trace)
    SHORT_HELP = 'Set tracing of various sorts.'
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
  command = Trepan::SubSubcommand::SetTrace.new(dbgr.core.processor,
                                                  set_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['set', name]
  set_cmd.instance_variable_set('@last_args', cmd_args)
  # require_relative '../../../lib/trepanning'
  # Trepan.debug
  command.run(cmd_args)
  command.run(['set', name, '*'])
end
