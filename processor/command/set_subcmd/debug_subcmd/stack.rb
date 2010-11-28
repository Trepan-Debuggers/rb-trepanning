# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetDebugStack < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP = "Set display of complete stack including possibly setup stack from trepanning"
    MIN_ABBREV  = 'st'.size
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %w(set debug stack)
  end

  def run(args)
    super
    @proc.hide_level  = 
      if @proc.settings[:debugstack]
        0
      else
        @proc.hidelevels[Thread.current] || 0
      end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../debug'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetDebug,
                                   Trepan::SubSubcommand::SetDebugStack)
  %w(off on 0 1).each { |arg| cmd.run([cmd.name, arg]) }
  puts '-' * 10
end
