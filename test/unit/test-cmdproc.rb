#!/usr/bin/env ruby
require 'test/unit'
require_relative File.join(%w(.. .. processor cmdproc))

# Test Debugger:CmdProcessor
class TestCmdProcessor < Test::Unit::TestCase

  def test_command_load
    dbg = Debugger::CmdProcessor.new()

    # See that we have commands
    cmds = dbg.instance_variable_get('@commands')
    assert cmds.is_a?(Hash), 'Should have gotten a command hash'
    assert cmds.keys.size > 0, 'Should have found at least one command'
    cmd_name, cmd_obj = cmds.first
    assert cmd_name.is_a?(String), 'Should have string name for a command'
    assert(cmd_obj.kind_of?(Debugger::Command), 
           'Command should be a Debugger:Command')
    assert cmd_obj.respond_to?(:run), 'Command should have a run method'
  end
end
