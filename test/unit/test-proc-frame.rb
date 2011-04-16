#!/usr/bin/env ruby
require 'test/unit'
require 'thread_frame'
require_relative '../../processor/main' # Have to include before frame!
                                        # FIXME
require_relative '../../processor/frame'
require_relative '../../app/mock'

$errors = []
$msgs   = []

# Test Trepan::CmdProcessor Frame portion
class TestCmdProcessorFrame < Test::Unit::TestCase

  def setup
    $errors = []
    $msgs   = []
    @proc    = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    @proc.frame_index = 0
    @proc.frame_initialize
    class << @proc
      def msg(msg)
        $msgs << msg
      end
      def errmsg(msg)
        $errors << msg
      end
      def print_location
        # $msgs << "#{@frame.source_container} #{@frame.source_location[0]}"
        $msgs << "#{@frame.source_container} "
        # puts $msgs
      end
    end
  end

  # See that we have can load up commands
  def test_basic
    @proc.frame_setup(RubyVM::ThreadFrame.current)

    # Test absolute positioning. Should all be okay
    0.upto(@proc.top_frame.stack_size-1) do |i| 
      @proc.adjust_frame(i, true) 
      assert_equal(0, $errors.size)
      assert_equal(true, $msgs.size >= i+1)
    end

    # Test absolute before the beginning fo the stack
    frame_index = @proc.frame_index
    @proc.adjust_frame(-1, true)
    assert_equal(0, $errors.size)
    assert_equal(frame_index, @proc.frame_index)
    @proc.adjust_frame(-@proc.top_frame.stack_size-1, true)
    assert_equal(1, $errors.size, $errors)
    assert_equal(frame_index, @proc.frame_index)

    setup
    @proc.top_frame  = @proc.frame = RubyVM::ThreadFrame.current
    @proc.adjust_frame(0, true)

    @proc.top_frame.stack_size-1.times do 
      frame_index = @proc.frame_index
      @proc.adjust_frame(1, false) 
      assert_equal(0, $errors.size)
      assert_not_equal(frame_index, @proc.frame_index,
                       '@proc.frame_index should have moved')
    end
    # FIXME: bug in threadframe top_frame.stack_size? 
    # # Adjust relative beyond the end
    # @proc.adjust_frame(1, false) 
    # assert_equal(1, $errors.size)

    # Should have stayed at the end
    # proc.adjust_frame(proc.top_frame.stack_size-1, true)
    # proc.top_frame.stack_size.times { proc.adjust_frame(-1, false) }

  end


end
