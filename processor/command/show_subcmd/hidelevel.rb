# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'

class Trepan::Subcommand::ShowHidelevel < Trepan::ShowIntSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = 'Show the number of stack levels to hide'
    MIN_ABBREV   = 'hide'.size
  end
  def run(args)
    if @proc.settings[:hidelevel]
      super
    else
      help = self.class.const_get(:HELP)
      msg "%s is auto selection." % help[5..-1].capitalize
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Trepan::Subcommand::ShowHidelevel.new(cmd)

  subcommand.run(subcommand.name)
  subcommand.proc.settings[:hidelevel] = 1
  subcommand.run(subcommand.name)
  
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new
  # dbgr.debugger
  subcommand.summary_help(subcommand.name)
  puts
  puts '-' * 20
end
