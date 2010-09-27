# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetMaxString < Trepan::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Set max st[ring] NUMBER

Sometimes the string representation of an object is very long. This
setting limits how much of the string representation you want to
see. However if the string has an embedded newline then we will assume
the output is intended to be formated as is.
'
    DEFAULT_MIN  = 10
    MIN_ABBREV   = 'str'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set max string)
    SHORT_HELP   = "Set maximum # chars in a string before truncation"
  end

  def run(args)
    args.shift
    args = %W(#{DEFAULT_MIN}) if args.empty?
    run_set_int(args.join(' '),
                "The '#{PREFIX.join(' ')}' command requires number at least #{DEFAULT_MIN}", 
                DEFAULT_MIN, nil)
  end

  alias save_command save_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Trepan::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                    set_cmd)
  # FIXME: remove the 'join' below
  cmd_name      = Trepan::SubSubcommand::SetMaxString::PREFIX.join('')
  subcmd        = Trepan::SubSubcommand::SetMaxString.new(set_cmd.proc, 
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
  puts subcmd.save_command
end
