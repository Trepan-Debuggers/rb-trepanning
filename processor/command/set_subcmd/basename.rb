# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetBasename < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = "Set to show only file basename in showing file names"
    IN_LIST    = true
    MIN_ABBREV = 'ba'.size
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this agin
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetBasename, false)
  cmd.run(cmd.prefix + ['off'])
  cmd.run(cmd.prefix + ['ofn'])
  cmd.run(cmd.prefix)
  puts cmd.save_command
end
