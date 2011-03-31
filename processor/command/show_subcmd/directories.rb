# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowDirectories < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = "Show current search path for finding source files"
    MIN_ABBREV   = 'dir'.size
  end

  def run(args)
    msg "Source directories searched: #{settings[:directory]}"
  end
end

if __FILE__ == $0 && caller.size == 0 
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::ShowDirectories)
end
