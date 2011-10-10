# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::ShowTracePrint < Trepan::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP         = "Show tracing print (set -x) status"
    MIN_ABBREV   = 'p'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show trace print)
    SHORT_HELP   = "Show tracing print (set -x) status"
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../trace'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::ShowTrace,
                                   Trepan::SubSubcommand::ShowTracePrint)
end
