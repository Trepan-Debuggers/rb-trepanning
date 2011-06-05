# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetAbbrev < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Set to allow unique abbreviations of commands"
    IN_LIST    = true
    MIN_ABBREV = 'ab'.size
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this again
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetAbbrev, false)
  cmd.run(cmd.prefix + ['off'])
  cmd.run(cmd.prefix + ['ofn'])
  cmd.run(cmd.prefix)
  puts cmd.save_command
end
