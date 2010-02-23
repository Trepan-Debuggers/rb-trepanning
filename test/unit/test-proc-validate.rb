#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. processor main) # Have to include before validate!
                                          # FIXME
require_relative %w(.. .. processor validate)
require_relative %w(.. .. app mock)

$errors = []
$msgs   = []

# Test Debugger:CmdProcessor Validation portion
class TestValidate < Test::Unit::TestCase

  def setup
    $errors = []
    $msgs   = []
    @proc    = Debugger::CmdProcessor.new(Debugger::MockCore.new())

    class << @proc
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
    onoff = 
    [['1', true],  ['on', true],
     ['0', false], ['off', false]].each do |arg, expected|
      assert_equal(expected, @proc.get_onoff(arg))
    end
    [['1', 1],  ['1E', nil], ['bad', nil], ['1+1', 2], ['-5', -5]].each do 
      |arg, expected|
      assert_equal(expected, @proc.get_int_noerr(arg))
    end
  end

  def test_int_list
    assert_equal([1,2,3], @proc.get_int_list(%w(1+0 3-1 3)))
    assert_equal(0, $errors.size)
    assert_equal([2,3], @proc.get_int_list(%w(a 2 3)))
    assert_equal(1, $errors.size)
  end

  def test_breakpoint_position
    require 'thread_frame'
    tf = RubyVM::ThreadFrame.current
    @proc.frame_setup(tf)

    def munge(args)
      args[1] = 'bogus'
      args
    end

    assert_equal([0, 'bogus', true, 'true'],
                 munge(@proc.breakpoint_position(%w(O0))))
    assert_equal([1, 'bogus', false, 'true'], 
                 munge(@proc.breakpoint_position(%w(1))))
    assert_equal([2, 'bogus', false, 'a > b'],
                 munge(@proc.breakpoint_position(%w(2 if a > b))))
  end
end
