# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::SetMaximumString < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Set max[imum] st[ring] NUMBER

Sometimes the string representation of an object is very long. This
setting limits how much of the string representation you want to
see. However if the string has an embedded newline then we will assume
the output is intended to be formated as is.
'
    IN_LIST      = true
    MIN_ABBREV   = 'st'.size
    NAME         = File.basename(__FILE__, '.rb')
    DEFAULT_MIN  = 10
    PREFIX       = %w(set maximum string)
    SHORT_HELP   = "Set maximum # chars in a string before truncation"
  end

  def run(args)
    args.shift
    args = %W(#{DEFAULT_MIN}) if args.empty?
    run_set_int(args.join(' '),
                "The 'set maximum string' command requires number at least 10", 
                DEFAULT_MIN, nil)
  end

  alias restore_command restore_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Debugger::SubSubcommand::SetMaximum.new(dbgr.core.processor, 
                                                          set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Debugger::SubSubcommand::SetMaximumString::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetMaximumString.new(set_cmd.proc, 
                                                                max_cmd,
                                                                cmd_name)
  subcmd.run([])
  subcmd.run(%w(0))
  subcmd.run(%w(20))
  subcmd.run(%w(100))
  name = File.basename(__FILE__, '.rb')
  subcmd.summary_help(name)
  puts
  puts '-' * 20
  puts subcmd.restore_command()
end
