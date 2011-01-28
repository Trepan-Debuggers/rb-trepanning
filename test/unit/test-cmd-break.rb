#!/usr/bin/env ruby
require_relative 'cmd-helper'

class TestCommandBreak < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name   = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd = @cmds[@name]
  end
  
  def test_basic
    tf = RubyVM::ThreadFrame.current
    @cmdproc.frame_setup(tf)
    [
     [@name,  __LINE__.to_s],
    ].each_with_index do |args, i|
      @my_cmd.run(args)
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint \d+ set at line \d+ in file #{__FILE__},\n\tVM offset \d+ of instruction sequence \"test_basic\"\.$/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
    pc_offset = tf.pc_offset
    [[@name],
     [@name, "O#{pc_offset}"],
     #[@name, 'FileUtils.cp']
    ].each_with_index do |args, i|
      @my_cmd.run(args)
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint \d+ set at VM offset \d+ of instruction sequence \"test_basic\",\n\tline \d+ in file .+\.$/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end

    def foo
      5 
    end
    [[@name, 'foo', (__LINE__-3).to_s]].each_with_index do |args, i|
      @my_cmd.run(args)
      assert_equal(true, @cmdproc.errmsgs.empty?,
                   @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint \d+ set at line \d+ in file #{__FILE__},\n\tVM offset \d+ of instruction sequence \"foo\"\.$/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
    [[@name, 'foo']].each_with_index do |args, i|
      @my_cmd.run(args)
      assert_equal(true, @cmdproc.errmsgs.empty?,
                   @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /Breakpoint \d+ set at line \d+ in file #{__FILE__},\n\tVM offset \d+/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
  end

  # Test setting a breakpoint at a line that can only be found using
  # the parent instruction sequence, i.e. the line is not in the
  # current instruction sequence.
  def test_parent_breakpoint
    xx = 5  # This is the line we set the breakpoint for.
    1.times do
      tf = RubyVM::ThreadFrame.current  
      @cmdproc.frame_setup(tf)
      @my_cmd.run([@name, (__LINE__-4).to_s])
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint \d+ set at line \d+ in file .+\n\tVM offset \d+ of instruction sequence \"test_parent_breakpoint\"\.$/,
                   @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
  end

end
