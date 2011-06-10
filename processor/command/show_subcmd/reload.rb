# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'rubygems'; require 'require_relative'
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowReload < Trepan::ShowBoolSubcommand
  unless defined?(SHORT_HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = "Show whether to reread source text when it changes"
    MIN_ABBREV = 're'.size
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::ShowReload, false)
end
