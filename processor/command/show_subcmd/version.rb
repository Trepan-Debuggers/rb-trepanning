# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/options'

class Trepan::Subcommand::ShowVersion < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = "Show debugger name and version"
  end

  def run(args)
    msg Trepan.show_version
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr, parent_cmd = MockDebugger::setup('show')
  cmd = Trepan::SubSubcommand::ShowVersion.new(parent_cmd)
  cmd.run(cmd.prefix)
end
