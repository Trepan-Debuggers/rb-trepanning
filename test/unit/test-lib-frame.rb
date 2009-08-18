#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. lib frame)
require 'thread_frame'

# Test Debugger:CmdProcessor
class TestLibFrame < Test::Unit::TestCase

  include Debugger::Frame

  def test_lib_frame
    frame = RubyVM::ThreadFrame.current
    base_count = frame.stack_size
    s = format_stack_entry(frame)
    pat = /^METHOD .*#test_lib_frame\(\) in file .*test-lib-frame.rb at line \d+/
    assert(s =~ pat, "got #{s}, expected pat #{pat}")
    1.times do 
      assert_equal(base_count+2, RubyVM::ThreadFrame.current.stack_size)
      s = format_stack_entry(frame)
      assert(s =~ pat, "got #{s}, expected pat #{pat}")
    end

    def foo(count)
      frame = RubyVM::ThreadFrame.current
      assert_equal(count, frame.stack_size)
      s = format_stack_entry(frame)
      pat = /^METHOD .*#foo\(count\) in file .*test-lib-frame.rb at line \d+/
      assert(s =~ pat, "got #{s}, expected pat #{pat}")
    end
    foo(base_count+1)
  end
end
