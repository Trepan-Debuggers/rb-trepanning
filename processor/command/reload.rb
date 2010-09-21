# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/submgr'

class Trepan::Command::ReloadCommand < Trepan::SubcommandMgr
  ALIASES       = %w(rel)
  HELP = 'Reload information'
  CATEGORY      = 'data'
  NAME          = File.basename(__FILE__, '.rb')
  NEED_STACK    = false
  SHORT_HELP    = 'Reload information'
  def initialize(proc)
    super
  end

end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  # require 'rbdbgr'
  # Trepan.debug(:set_restart => true)
  xx = Trepan::Command::ReloadCommand.new(cmd)
  cmd.run([name])
  cmd.run([name, 'command'])
end
