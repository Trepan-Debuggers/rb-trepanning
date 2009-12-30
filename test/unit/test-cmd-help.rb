#!/usr/bin/env ruby
require 'test/unit'
require_relative 'cmd-helper'
require_relative %w(.. .. processor command help)

class TestCommandHelp < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name     = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd   = @cmds[@name]
  end
  
  # Test we can run 'help *cmd* for each command
  def test_help_command
    @cmds.each do |cmd_name, cmd|
      @cmdproc.instance_variable_set('@msgs', [])
      @my_cmd.run([@name, cmd_name])
      assert_equal(false,  @cmdproc.instance_variable_get('@msgs').empty?)
    end
  end

end
