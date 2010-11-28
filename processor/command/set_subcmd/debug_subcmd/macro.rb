# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetDebugMacro < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set macro debugging"
    MIN_ABBREV  = 'macro'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %W(set debug #{NAME})
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../debug'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetDebug,
                                   Trepan::SubSubcommand::SetDebugMacro)
  %w(off on 0 1).each { |arg| cmd.run([cmd.name, arg]) }
  puts '-' * 10
  puts cmd.save_command.join("\n")
end
