#!/usr/bin/env ruby
require_relative 'cmd-helper'

class TestCommandBreak < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name   = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd = @cmds[@name]
    @brkpt_set_pat = /^Breakpoint \d+ set at VM offset \d+ of instruction sequence .*,\n\tline \d+ in file .*$/ 
  end

  def run_cmd(cmd, args) 
    cmd.proc.instance_variable_set('@cmd_argstr', args[1..-1].join(' '))
    cmd.run(args)
  end

  # require_relative '../../lib/trepanning'
  def test_basic
    tf = RubyVM::Frame.current
    @cmdproc.frame_setup(tf)
    [
     [@name,  __LINE__.to_s],
    ].each_with_index do |args, i|
      run_cmd(@my_cmd, args)
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, @cmdproc.msgs[0] =~ @brkpt_set_pat, @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
    pc_offset = tf.pc_offset
    [[@name],
     [@name, "@#{pc_offset}"],
     #[@name, 'FileUtils.cp']
    ].each_with_index do |args, i|
      run_cmd(@my_cmd, args)
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, @cmdproc.msgs[0] =~ @brkpt_set_pat, @cmdproc.msgs[0])
      reset_cmdproc_vars
    end

    def foo
      5 
    end
    [[@name, 'foo', (__LINE__-3).to_s]].each_with_index do |args, i|
      run_cmd(@my_cmd, args)
      assert_equal(true, @cmdproc.errmsgs.empty?,
                   @cmdproc.errmsgs)
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      reset_cmdproc_vars
    end
    [[@name, 'foo']].each_with_index do |args, i|
      run_cmd(@my_cmd, args)
      assert_equal(true, @cmdproc.errmsgs.empty?,
                   @cmdproc.errmsgs)
      assert_equal(0, @cmdproc.msgs[0] =~ @brkpt_set_pat, @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
  end

  # Test setting a breakpoint at a line that can only be found using
  # the parent instruction sequence, i.e. the line is not in the
  # current instruction sequence.
  def test_parent_breakpoint
    # skip "Need to add ISEQ parent link to 1.9.2 and further check of old code" 
    # require_relative '../../lib/trepanning'
    line = __LINE__  # This is the line we set the breakpoint for.
    1.times do
      tf = RubyVM::Frame.current  
      @cmdproc.frame_setup(tf)
      run_cmd(@my_cmd, [@name, line.to_s])
      assert_equal(true, @cmdproc.errmsgs.empty?, @cmdproc.errmsgs)
      assert_equal(0, @cmdproc.msgs[0] =~ @brkpt_set_pat, @cmdproc.msgs[0])
      reset_cmdproc_vars
    end
  end

end
