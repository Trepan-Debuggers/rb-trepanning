#!/usr/bin/env ruby
require 'test/unit'
require_relative 'cmd-helper'

class TestCommandBreak < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name   = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd = @cmds[@name]
  end
  
  def test_basic
    require 'thread_frame'
    tf = RubyVM::ThreadFrame.current
    @cmdproc.frame_setup(tf)
    pc_offset = tf.pc_offset
    [[@name],
     [@name,  __LINE__.to_s],
     [@name, "O#{pc_offset}"],
    ].each_with_index do |args, i|
      @my_cmd.run(args)
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint #{i+1} set at line \d+\n\tin file .*\n\tVM offset \d+ of instruction sequence \"test_basic\"\.$/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end

    def foo
      5 
    end
    [[@name, 'foo'],
     [@name, 'foo', (__LINE__-3).to_s]
    ].each_with_index do |args, i|
      @my_cmd.run(args)
      assert_equal(true, @cmdproc.errmsgs.empty?,
                   @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint #{i+4} set at line \d+\n\tin file .*\n\tVM offset \d+ of instruction sequence \"foo\"\.$/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
  end

end
