#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/breakpoint'
require 'thread_frame'

class TestAppBrkpt < Test::Unit::TestCase

  def test_basic
    tf = RubyVM::ThreadFrame.current
    iseq = tf.iseq
    b1 = Trepan::Breakpoint.new(iseq, 0)
    assert_equal(false, b1.temp?)
    assert_equal(0, b1.hits)
    assert_equal('B', b1.icon_char)
    assert_equal(true, b1.condition?(tf.binding))
    assert_equal(1, b1.hits)
    assert_equal(b1.source_container, tf.source_container)
    b1.enabled = false
    assert_equal('b', b1.icon_char)
    assert_raises TypeError do 
      Trepan::Breakpoint.new(iseq, iseq.iseq_size, :temp => true)
    end
    assert_raises TypeError do 
      Trepan::Breakpoint.new(0, 5)
    end
    require_relative '../../lib/trepanning.rb'
    b2 = Trepan::Breakpoint.new(iseq, 0, :temp => true)
    assert_equal('t', b2.icon_char)
  end
end
