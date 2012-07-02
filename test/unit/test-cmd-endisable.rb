#!/usr/bin/env ruby
require_relative 'cmd-helper'
require_relative '../../app/breakpoint'

class TestCommandEnableDisable < Test::Unit::TestCase

  include UnitHelper
  def setup
    Trepan::Breakpoint::reset
    common_setup
    @break_cmd   = @cmds['break']
    @disable_cmd = @cmds['disable']
    @enable_cmd  = @cmds['enable']
  end

  def test_basic

    # Some simple errors in running enable/disable commands
    [[@enable_cmd, ['enable', '1'], 0, 1],
     [@disable_cmd, ['disable', '1'], 0, 1]].each do 
      |cmd, args, nmsgs, nerrs|
      # No breakpoint number given
      cmd.run(args[0..0])
      assert_equal(nmsgs, @cmdproc.msgs.size, 
                   "#{args} - msgs: #{@cmdproc.msgs.inspect}")
      assert_equal(nerrs, @cmdproc.errmsgs.size, 
                   "#{args} - errmsgs: #{@cmdproc.errmsgs.inspect}")
      reset_cmdproc_vars

      # Non-existent breakpoint
      cmd.run(args)
      assert_equal(true, @cmdproc.msgs.empty?)
      assert_equal(1, @cmdproc.errmsgs.size)
      reset_cmdproc_vars
    end

    tf = RubyVM::Frame.current
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
