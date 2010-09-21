#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/frame'
require 'thread_frame'

class TestAppFrame < Test::Unit::TestCase

  include Trepan::Frame

  def test_app_frame
    frame = RubyVM::ThreadFrame.current
    base_count = frame.stack_size
    s = format_stack_entry(frame)
    pat = /^METHOD .*#test_app_frame\(\) in file .*test-app-frame.rb at line \d+/
    assert(s =~ pat, "got #{s}, expected pat #{pat}")
    1.times do 
      assert_equal(base_count+2, RubyVM::ThreadFrame.current.stack_size)
      s = format_stack_entry(frame)
      assert(s =~ pat, "got #{s}, expected pat #{pat}")
    end

    def inner_test(count)
      frame = RubyVM::ThreadFrame.current
      assert_equal(count, frame.stack_size)

      s = format_stack_entry(frame)
      pat = /^METHOD .*#inner_test\(count\) in file .*test-app-frame.rb at line \d+/
      assert(s =~ pat, "got: #{s.inspect},\nexpected: pat #{pat}")

      s = format_stack_entry(frame, :basename => true)
      pat = /^METHOD .*#inner_test\(count\) in file test-app-frame.rb at line \d+/
      assert(s =~ pat, "got: #{s.inspect},\nexpected: pat #{pat}")

      s = format_stack_entry(frame, :basename => true, :show_pc => true)
      pat = /^METHOD .*#inner_test\(count\) in file test-app-frame.rb at line \d+, pc: \d+/
      assert(s =~ pat, "got: #{s.inspect},\nexpected: pat #{pat}")
    end
    inner_test(base_count+1)
  end

  def test_return
    assert_equal(1, offset_for_return('return'))
    assert_equal(2, offset_for_return('c-return'))
    assert_raises RuntimeError do
      offset_for_return('c-call')
    end
  end

end
