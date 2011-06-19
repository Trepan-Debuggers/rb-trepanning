# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/submgr'

class Trepan::Command::InfoCommand < Trepan::SubcommandMgr
  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
Generic command for showing things about the program being debugged. 

You can give unique prefix of the name of a subcommand to get
information about just that subcommand.

Type "#{NAME}" for a list of "info" subcommands and what they do.
Type "help #{NAME} *" for just a list of "info" subcommands.
    HELP

    ALIASES       = %w(i)
    CATEGORY      = 'status'
    SHORT_HELP    = 'Information about debugged program and its environment'
  end
end

if __FILE__ == $0
  require_relative '../mock'
  require 'ruby-debug'; Debugger.start; debugger
  dbgr, cmd = MockDebugger::setup
  cmd.run(['info', 'iv'])
end
