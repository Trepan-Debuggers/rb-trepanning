#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/util'

class TestAppUtil < Test::Unit::TestCase
  include Trepan::Util
  def test_safe_repr
    string = 'The time has come to talk of many things.'
    assert_equal(string, safe_repr(string, 50))
    assert_equal('The time...  things.', safe_repr(string, 17))
    assert_equal('"The tim... things."', safe_repr(string.inspect, 17))
    string = "'The time has come to talk of many things.'"
    assert_equal("'The tim... things.'", safe_repr(string, 17))
  end
end
