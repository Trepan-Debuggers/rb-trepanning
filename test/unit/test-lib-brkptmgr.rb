#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. lib brkptmgr)
require_relative %w(.. .. lib brkpt)
require 'thread_frame'

class TestLibBrkptMgr < Test::Unit::TestCase

  def test_basic
    tf = RubyVM::ThreadFrame.current
    iseq = tf.iseq
    offsets = iseq.offsetlines.keys
    offset  = offsets[0]
    brkpts = BreakpointMgr.new
    assert_equal(0, brkpts.size)
    b1 = brkpts.add(false, offset, iseq)
    assert_equal(b1, brkpts.find(iseq, offset, tf.binding))
    assert_equal(1, brkpts.size)
    assert_equal(b1, brkpts.find(iseq, offset, tf.binding))
    assert_equal(b1, brkpts.delete(b1.id))
    assert_equal(0, brkpts.size)
    b2 = brkpts << Breakpoint.new(true, offsets[1], iseq)

    assert_equal(nil, brkpts.find(iseq, offset, tf.binding))
    brkpts.reset
    assert_equal(0, brkpts.size)
  end
end
