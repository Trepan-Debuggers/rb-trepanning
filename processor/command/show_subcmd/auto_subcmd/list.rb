# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::ShowAutoList < Trepan::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show running a 'list' command each time we enter the debugger"
    MIN_ABBREV   = 'l'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show auto list)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../auto'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::ShowAuto,
                                   Trepan::SubSubcommand::ShowAutoList)
end
