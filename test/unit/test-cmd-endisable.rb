#!/usr/bin/env ruby
require 'test/unit'
require_relative 'cmd-helper'

class TestCommandEnableDisable < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @break_cmd   = @cmds['break']
    @disable_cmd = @cmds['disable']
    @enable_cmd  = @cmds['enable']
  end
  
  def test_basic

    # Some simple errors in running enable/disable commands
    [[@enable_cmd, ['enable', '1']],
     [@disable_cmd, ['disable', '1']]].each do |cmd, args|
      # No breakpoint number given
      cmd.run(args[0..0])
      assert_equal(true, @cmdproc.msgs.empty?)
      assert_equal(1, @cmdproc.errmsgs.size)
      reset_cmdproc_vars

      # Non-existent breakpoint
      cmd.run(args)
      assert_equal(true, @cmdproc.msgs.empty?)
      assert_equal(1, @cmdproc.errmsgs.size)
      reset_cmdproc_vars
    end

    require 'thread_frame'
    tf = RubyVM::ThreadFrame.current
    @cmdproc.frame_setup(tf)
    pc_offset = tf.pc_offset
    @break_cmd.run(['break'])
    assert_equal(true, @cmdproc.errmsgs.empty?)
    assert_equal(1, @cmdproc.msgs.size)
    reset_cmdproc_vars

    @disable_cmd.run(['disable', '1'])
    assert_equal(0, 
                 @cmdproc.msgs[0] =~ /^Breakpoint 1 disabled./,
                 @cmdproc.msgs)
    reset_cmdproc_vars

    @enable_cmd.run(['enable', '1'])
    assert_equal(0, 
                 @cmdproc.msgs[0] =~ /^Breakpoint 1 enabled./,
                 @cmdproc.msgs)
  end

end
