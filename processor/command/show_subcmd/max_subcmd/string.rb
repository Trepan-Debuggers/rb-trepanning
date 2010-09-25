# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::ShowMaxString < Trepan::ShowIntSubSubcommand
  unless defined?(HELP)
    HELP = 'Show the number of characters in a string before truncating.

Sometimes the string representation of an object is very long. This
setting limits how much of the string representation you want to
see. However if the string has an embedded newline then we will assume
the output is intended to be formated as.'
    MIN_ABBREV   = 'st'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show max string)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Trepan::Subcmd.new(show_cmd)
  max_cmd        = Trepan::SubSubcommand::ShowMax.new(dbgr.core.processor, 
                                                      show_cmd)
  
  cmd_name       = Trepan::SubSubcommand::ShowMaxString::PREFIX.join('')
  maxx_cmd       = Trepan::SubSubcommand::ShowMaxString.new(show_cmd.proc,
                                                            max_cmd,
                                                            cmd_name)

  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  puts max_cmd.summary_help(maxx_cmd)
  puts
  maxx_cmd.run([])
end
