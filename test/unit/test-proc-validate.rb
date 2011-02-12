#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor/main' # Have to include before validate!
                                        # FIXME
require_relative '../../processor/validate'
require_relative '../../app/mock'
require 'thread_frame'

$errors = []
$msgs   = []

# Test Trepan::CmdProcessor Validation portion
class TestValidate < Test::Unit::TestCase

  def setup
    $errors = []
    $msgs   = []
    @proc    = Trepan::CmdProcessor.new(Trepan::MockCore.new())

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

  def test_breakpoint_position
    tf = RubyVM::ThreadFrame.current
    @proc.frame_setup(tf)

    def munge(args)
      args[1] = 'bogus'
      args
    end

    assert_equal([0, 'bogus', true, 'true', nil],
                 munge(@proc.breakpoint_position(%w(O0))))
    assert_equal([1, 'bogus', false, 'true', nil], 
                 munge(@proc.breakpoint_position(%w(1))))
    assert_equal([2, 'bogus', false, 'a > b', nil],
                 munge(@proc.breakpoint_position(%w(2 if a > b))))
  end

  def test_int_list
    assert_equal([1,2,3], @proc.get_int_list(%w(1+0 3-1 3)))
    assert_equal(0, $errors.size)
    assert_equal([2,3], @proc.get_int_list(%w(a 2 3)))
    assert_equal(1, $errors.size)
  end

  def test_parse_position
    tf = RubyVM::ThreadFrame.current
    @proc.frame_setup(tf)
    assert_equal(8, @proc.parse_position('8')[-1])
    assert_equal(0, $errors.size)
  end

  def test_method?
    def foo; 5 end
    
    # require_relative '../../lib/trepanning'
    # debugger
    # FIXME: 'foo', 'setup'
    require 'irb'
    %w(Array.map errmsg Trepan::CmdProcessor.new IRB.start).each do |str|
      assert @proc.method?(str), "#{str} should be known as a method"
    end
    ['food', '.errmsg'
    ].each do |str|
      # dbgr.debugger if 'foo' == str
      assert_equal(false, !!@proc.method?(str),
                   "#{str} should not be known as a method")
    end

  end

end
