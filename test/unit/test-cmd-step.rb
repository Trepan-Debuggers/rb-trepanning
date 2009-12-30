#!/usr/bin/env ruby
require 'test/unit'
require_relative 'cmd-helper'
require_relative %w(.. .. processor command step)

class TestCommandStep < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name     = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd   = @cmds[@name]
  end

  def reset_cmdproc_vars
    @cmdproc.instance_variable_set('@msgs', [])
    @cmdproc.instance_variable_set('@errmsgs', [])
    @cmdproc.leave_cmd_loop = false
  end
  
  def test_step_command
    @my_cmd.run([@name, 'foo'])
    assert_equal(false,  @cmdproc.leave_cmd_loop)

    reset_cmdproc_vars
    @my_cmd.run([@name, '5'])
    assert_equal(true,  @cmdproc.leave_cmd_loop)
    assert_equal(4,  @core.step_count)

    reset_cmdproc_vars
    @my_cmd.run([@name])
    assert_equal(true,  @cmdproc.leave_cmd_loop)
    assert_equal(0,  @core.step_count)

    reset_cmdproc_vars
    @my_cmd.run([@name, '1+(2*3)'])
    assert_equal(true,  @cmdproc.leave_cmd_loop)
    assert_equal(6,  @core.step_count)

    reset_cmdproc_vars
    @my_cmd.run([@name, '1+foo'])
    assert_equal(false,  @cmdproc.leave_cmd_loop)
  end

end
