#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/brkptmgr'
require_relative '../../app/brkpt'
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

    # Try adding via << rather than .add
    b2 = brkpts << Breakpoint.new(true, offsets[1], iseq)

    assert_equal(nil, brkpts.find(iseq, offset, tf.binding))
    brkpts.reset
    assert_equal(0, brkpts.size)
  end

  def test_multiple_brkpt_per_offset
    tf = RubyVM::ThreadFrame.current
    iseq = tf.iseq
    offsets = iseq.offsetlines.keys
    offset  = offsets[0]
    brkpts = BreakpointMgr.new
    b1 = brkpts.add(false, offset, iseq)
    b2 = brkpts.add(false, offset, iseq)
    assert_equal(2, brkpts.size)
    assert_equal(1, brkpts.set.size, 
                 'Two breakpoints but only one iseq/offset')
    brkpts.delete_by_brkpt(b1)
    assert_equal(1, brkpts.size, 
                 'One breakpoint after 2nd breakpoint deleted')
    assert_equal(1, brkpts.set.size, 
                 'Two breakpoints, but only one iseq/offset')
    brkpts.delete_by_brkpt(b2)
    assert_equal(0, brkpts.size, 
                 'Both breakpoints deleted')
    assert_equal(0, brkpts.set.size, 
                 'Second breakpoint delete should delete iseq/offset')
  end

end
