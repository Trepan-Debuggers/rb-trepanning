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
    @cmdproc    = Trepan::CmdProcessor.new(Trepan::MockCore.new())

    class << @cmdproc
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
      assert_equal(expected, @cmdproc.get_int_noerr(arg))
    end
  end

  def test_get_on_off
    onoff = 
    [['1', true],  ['on', true],
     ['0', false], ['off', false]].each do |arg, expected|
      assert_equal(expected, @cmdproc.get_onoff(arg))
    end
  end

  def test_parse_position
    tf = RubyVM::ThreadFrame.current
    @cmdproc.frame_setup(tf)
    file = File.basename(__FILE__)
    [[__FILE__, [true, file, nil, nil]],
     ['@8', [true, file, 8, :offset]],
     ['8' , [true, file, 8, :line]],
     ["#{__FILE__}:#{__LINE__}" , [true, file, __LINE__, :line]],
     ["#{__FILE__} #{__LINE__}" , [true, file, __LINE__, :line]]
    ].each do |pos_str, expected|
      result = @cmdproc.parse_position(pos_str)
      result[1] = File.basename(result[1])
      result[0] = !!result[0]
      assert_equal(expected, result, "parsing position #{pos_str}")
    end
  end

  def test_file_exists_proc
    load 'tmpdir.rb'
    %W(#{__FILE__} tmpdir.rb app/mock.rb).each do |name|
      assert_equal true, @cmdproc.file_exists_proc.call(name), "Should find #{name}"
    end
    %W(#{File.dirname(__FILE__)} tmpdir).each do |name|
      assert_equal false, @cmdproc.file_exists_proc.call(name), "Should not find #{name}"
    end
  end

  # def test_breakpoint_position
  #   tf = RubyVM::ThreadFrame.current
  #   @cmdproc.frame_setup(tf)

  #   def munge(args)
  #     args[1] = 'bogus'
  #     args
  #   end

  #   assert_equal([0, 'bogus', true, 'true', nil],
  #                munge(@cmdproc.breakpoint_position(%w(@0))))
  #   assert_equal([1, 'bogus', false, 'true', nil], 
  #                munge(@cmdproc.breakpoint_position(%w(1))))
  #   assert_equal([2, 'bogus', false, 'a > b', nil],
  #                munge(@cmdproc.breakpoint_position(%w(2 if a > b))))
  # end

  def test_int_list
    assert_equal([1,2,3], @cmdproc.get_int_list(%w(1+0 3-1 3)))
    assert_equal(0, $errors.size)
    assert_equal([2,3], @cmdproc.get_int_list(%w(a 2 3)))
    assert_equal(1, $errors.size)
  end

  def test_method?
    def foo; 5 end
    require 'irb'
    tf = RubyVM::ThreadFrame.current
    @cmdproc.frame_setup(tf)
    @cmdproc.method?('@cmdproc.errmsg')
    %w(Array.map @cmdproc.errmsg foo Trepan::CmdProcessor.new IRB.start 
      ).each do |str|
      assert @cmdproc.method?(str), "#{str} should be known as a method"
    end
    ['food', '.errmsg'
    ].each do |str|
      assert_equal(false, !!@cmdproc.method?(str),
                   "#{str} should not be known as a method")
    end

  end

end
