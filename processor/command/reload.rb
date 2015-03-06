# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/submgr'
require_relative '../../app/util'

class Trepan::Command::ReloadCommand < Trepan::SubcommandMgr
  Trepan::Util.suppress_warnings {
    NAME          = File.basename(__FILE__, '.rb')
    ALIASES       = %w(rel)
    CATEGORY      = 'data'
    NEED_STACK    = false
    SHORT_HELP    = 'Reload information'
    HELP = <<-HELP
**#{NAME}** [*reload-subcommand*]

Reload debugger information after something has changed.
For example if you change the Ruby code for a debugger *quit*,
`reload command quit` will get the change seen inside the
currently-running debugger.
   HELP
  }

  def initialize(proc)
    super
  end

end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup

  # require 'trepanning'
  # Trepan.debug
  xx = Trepan::Command::ReloadCommand.new(cmd)
  cmd.run([cmd.name])
  cmd.run([cmd.name, 'command'])
end
