# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/submgr'

class Trepan::Command::ReloadCommand < Trepan::SubcommandMgr
  NAME          = File.basename(__FILE__, '.rb')
  ALIASES       = %w(rel)
  HELP = 'Reload information'
  CATEGORY      = 'data'
  NEED_STACK    = false
  SHORT_HELP    = 'Reload information'
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
