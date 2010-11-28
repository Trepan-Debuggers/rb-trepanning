# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../auto'

class Trepan::SubSubcommand::ShowAutoIrb < Trepan::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Show if IRB is invoked on debugger stops"
    MIN_ABBREV = 'ir'.size
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(show auto irb)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../auto'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::ShowAuto,
                                   Trepan::SubSubcommand::ShowAutoIrb)
end
