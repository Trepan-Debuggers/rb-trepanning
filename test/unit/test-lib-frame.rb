#!/usr/bin/env ruby
require 'test/unit'
require_relative File.join(%w(.. .. lib frame))
require 'thread_frame'

# Test Debugger:CmdProcessor
class TestLibFrame < Test::Unit::TestCase

  include Debugger::Frame

  def test_lib_frame
    frame = RubyVM::ThreadFrame.current
    base_count = count_frames(frame)
    s = format_stack_entry(frame)
    pat = /^METHOD .*#test_lib_frame\(\) \["file", ".*test-lib-frame.rb"\] at line \d+/
    assert s =~ pat 
    1.times do 
      assert_equal(base_count+2, count_frames(RubyVM::ThreadFrame.current))
      s = format_stack_entry(frame)
      assert s =~ pat
    end

    def foo(count)
      frame = RubyVM::ThreadFrame.current
      assert_equal(count, count_frames(frame))
      s = format_stack_entry(frame)
      pat = /^METHOD .*#foo\(count\) \["file", ".*test-lib-frame.rb"\] at line \d+/
      assert s =~ pat
    end
    foo(base_count+1)
  end
end
