#!/usr/bin/env ruby
require 'test/unit'
require_relative File.join(%w(.. .. lib core))
require_relative File.join(%w(.. .. processor cmdproc))
require_relative File.join(%w(.. .. processor command help))

class TestCommandHelp < Test::Unit::TestCase

  def setup
    @errors             = []
    @msgs               = []
    @core = Debugger::Core.new()
    @dbg = Debugger::CmdProcessor.new(@core)
    @cmds = @dbg.instance_variable_get('@commands')
    @help_cmd = @cmds['help']
    def @help_cmd.msg(message)
      @msgs << message
    end
  end
  
  # Test we can run 'help *cmd* for each command
  def test_help_command
    @cmds.each do |cmd_name, cmd|
      @help_cmd.instance_variable_set('@msgs', [])
      @help_cmd.run(['help', cmd_name])
      assert @help_cmd.instance_variable_get('@msgs')
    end
  end

end
