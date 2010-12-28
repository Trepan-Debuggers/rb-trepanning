#!/usr/bin/env ruby
require_relative './mock-helper'
require_relative '../../processor/command/kill'

class TestCommandKill < Test::Unit::TestCase
  include MockUnitHelper
  def setup
    @name = File.basename(__FILE__, '.rb').split(/-/)[2]
    common_setup(@name)
    def @cmd.msg(message)
      @msgs << message
    end
    def @cmd.errmsg(message)
      @errmsgs << message
    end
    reset_cmdproc_vars
  end

  def reset_cmdproc_vars
    @cmd.instance_variable_set('@msgs', [])
    @cmd.instance_variable_set('@errmsgs', [])
    @cmd.proc.leave_cmd_loop = false
  end
  
  def test_kill_command
      @cmd.run([@name, 'foo'])
      assert_equal(false,  @cmd.proc.leave_cmd_loop)
      assert_equal(1, @cmd.instance_variable_get('@errmsgs').size)
      
    if false
      save_trap = Signal.trap(10) {
        @success = true
      }
      
      @success = false
      @cmd.run([@name, '10'])
      #  assert_equal(true,  @success, 
      #             'Should have run the handler')
    end

  ensure
    # restore old trap if any
    trap(10, save_trap)
  end

end
