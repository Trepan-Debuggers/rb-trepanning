#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/display'

class TestLibAppBrkptMgr < Test::Unit::TestCase

  def test_basic
    tf = RubyVM::Frame.get
    mgr = DisplayMgr.new
    x = 1
    assert_equal(0, mgr.size)
    disp = mgr.add(tf, 'x > 1')
    assert_equal(1, mgr.max)
    assert_equal(true, disp.enabled?)

    mgr.enable_disable(disp.number, false)
    assert_equal(false, disp.enabled?)
    mgr.enable_disable(disp.number, true)
    assert_equal(true, disp.enabled?)
  end
end
