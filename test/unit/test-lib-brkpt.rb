#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. app brkpt)
require 'thread_frame'

class TestLibBrkpt < Test::Unit::TestCase

  def test_basic
    tf = RubyVM::ThreadFrame.current
    iseq = tf.iseq
    b1 = Breakpoint.new(false, 0, iseq)
    assert_equal(false, b1.temp?)
    assert_equal(0, b1.hits)
    assert_equal('B', b1.icon_char)
    assert_equal(true, b1.condition?(tf.binding))
    assert_equal(1, b1.hits)
    assert_equal(b1.source_container, tf.source_container)
    b1.enabled = false
    assert_equal('b', b1.icon_char)
    assert_raises TypeError do 
      Breakpoint.new(true, iseq.iseq_size, iseq)
    end
    assert_raises TypeError do 
      Breakpoint.new(false, 0, 5)
    end
    require_relative %w(.. .. lib rbdbgr.rb)
    b2 = Breakpoint.new(true, 0, iseq)
    assert_equal('t', b2.icon_char)
  end
end
