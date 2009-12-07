#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. app core)
require_relative %w(.. .. app mock)
require_relative %w(.. .. processor main) # Have to include before frame!
                                          # FIXME
require_relative %w(.. .. processor frame) 

class TestCommandBreak < Test::Unit::TestCase

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
    def @cmdproc.errmsgs
      @errmsgs
    end
    def @cmdproc.msgs
      @msgs
    end
    reset_cmdproc_vars
  end
  
  def reset_cmdproc_vars
    @cmdproc.instance_variable_set('@msgs', [])
    @cmdproc.instance_variable_set('@errmsgs', [])
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
      assert_equal(true, @cmdproc.errmsgs.empty?)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint #{i+1} set at line \d+\n\tin file .*\n.* test_basic.$/,
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
      assert_equal(true, @cmdproc.errmsgs.empty?)
      assert_equal(0, 
                   @cmdproc.msgs[0] =~ /^Breakpoint #{i+4} set at line \d+\n\tin file .*\n.* foo.$/)
      reset_cmdproc_vars
    end
  end

end
