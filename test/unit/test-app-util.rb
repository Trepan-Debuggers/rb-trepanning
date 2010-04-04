#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/util'

class TestAppUtil < Test::Unit::TestCase
  include Rbdbgr
  def test_safe_repr
    string = 'The time has come to talk of many things.'
    assert_equal(string, safe_repr(string, 50))
    assert_equal('The time has come...', safe_repr(string, 17))
    assert_equal('The time has come', safe_repr(string, 17, ''))
    assert_equal('"The time has co"...', safe_repr(string.inspect, 17))
    string = "'The time has come to talk of many things.'"
    assert_equal("'The time has co'...", safe_repr(string, 17))
  end

end
