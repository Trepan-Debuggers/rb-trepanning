#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. app core)
require_relative %w(.. .. processor main)
require_relative %w(.. .. processor command help)

# Mock debugger stub. FIXME: put in comment helper routine.
class Debugger
end

class TestCommandStep < Test::Unit::TestCase

  def setup
    @dbg      = Debugger.new
    @core     = Debugger::Core.new(@dbg)
    @cmdproc  = @core.processor = Debugger::CmdProcessor.new(@core)
    @cmds     = @cmdproc.commands
    @name     = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd   = @cmds[@name]
    def @cmdproc.msg(message)
      @msgs << message
    end
    def @cmdproc.errmsg(message)
      @errmsgs << message
    end
    reset_cmdproc_vars
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
