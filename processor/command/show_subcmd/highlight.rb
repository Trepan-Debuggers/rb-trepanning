# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::Subcommand::ShowHighlight < Trepan::ShowBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Show whether we use terminal highlighting'
    MIN_ABBREV   = 'high'.size
  end

  def run(args)
    val = :term == @proc.settings[:highlight] 
    onoff = show_onoff(val)
    msg("%s is %s." % [@name, onoff])
  end
end

if __FILE__ == $0
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::ShowHighlight, false)
  cmd.run(cmd.prefix)
end
