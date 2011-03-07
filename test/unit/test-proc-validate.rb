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

  def test_get_int
    [['1', 1],  ['1E', nil], ['bad', nil], ['1+1', 2], ['-5', -5]].each do 
      |arg, expected|
      assert_equal(expected, @proc.get_int_noerr(arg))
    end
  end

  def test_get_on_off
    onoff = 
    [['1', true],  ['on', true],
     ['0', false], ['off', false]].each do |arg, expected|
      assert_equal(expected, @proc.get_onoff(arg))
    end
  end

  def test_parse_position
    tf = RubyVM::ThreadFrame.current
    @proc.frame_setup(tf)
    [[__FILE__, [false, __FILE__, nil, nil]],
     ['@8', [true, __FILE__, 8, :offset]],
     ['8' , [true, __FILE__, 8, :line]],
     ["#{__FILE__}:#{__LINE__}" , [false, __FILE__, __LINE__, :line]],
     ["#{__FILE__} #{__LINE__}" , [false, __FILE__, __LINE__, :line]]
    ].each do |pos_str, expected|
      result = @proc.parse_position(pos_str)
      result[0] = !!result[0]
      assert_equal(expected, result, "parsing position #{pos_str}")
    end
  end

  # def test_breakpoint_position
  #   tf = RubyVM::ThreadFrame.current
  #   @proc.frame_setup(tf)

  #   def munge(args)
  #     args[1] = 'bogus'
  #     args
  #   end

  #   assert_equal([0, 'bogus', true, 'true', nil],
  #                munge(@proc.breakpoint_position(%w(@0))))
  #   assert_equal([1, 'bogus', false, 'true', nil], 
  #                munge(@proc.breakpoint_position(%w(1))))
  #   assert_equal([2, 'bogus', false, 'a > b', nil],
  #                munge(@proc.breakpoint_position(%w(2 if a > b))))
  # end

  def test_int_list
    assert_equal([1,2,3], @proc.get_int_list(%w(1+0 3-1 3)))
    assert_equal(0, $errors.size)
    assert_equal([2,3], @proc.get_int_list(%w(a 2 3)))
    assert_equal(1, $errors.size)
  end

  def test_method?
    def foo; 5 end
    require 'irb'
    tf = RubyVM::ThreadFrame.current
    @proc.frame_setup(tf)
    @proc.method?('@proc.errmsg')
    %w(Array.map @proc.errmsg foo Trepan::CmdProcessor.new IRB.start 
      ).each do |str|
      assert @proc.method?(str), "#{str} should be known as a method"
    end
    ['food', '.errmsg'
    ].each do |str|
      assert_equal(false, !!@proc.method?(str),
                   "#{str} should not be known as a method")
    end

  end

end
