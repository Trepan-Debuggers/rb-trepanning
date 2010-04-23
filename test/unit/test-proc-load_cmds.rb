#!/usr/bin/env ruby
require 'test/unit'
require 'thread_frame'
require_relative '../../processor/load_cmds'
require_relative '../../app/mock'

class TestCmdProcessorLoadCmds < Test::Unit::TestCase

  def setup
    @proc = Debugger::CmdProcessor.new(Debugger::MockCore.new())
  end

  # See that we have can load up commands
  def test_basic
    @proc.load_cmds_initialize
    assert_equal(false, @proc.commands.empty?)
    assert_equal(false, @proc.aliases.empty?)
  end
end
