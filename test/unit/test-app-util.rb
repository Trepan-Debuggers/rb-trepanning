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

  def test_abbrev
    list = %w(disassemble disable distance up)
    [['dis', 'dis'],
     ['disas', 'disassemble'],
     ['u', 'up'],
     ['upper', 'upper'],
     ['foo', 'foo']].each do |name, expect|
      assert_equal expect, uniq_abbrev(list, name)
    end
  end

  def test_extract_expression
    [['if condition("if")',        'condition("if")'],
     ['until until_termination',   'until_termination'],
     ['return return_value',       'return_value'],
     ['nothing_to_be.done',         'nothing_to_be.done'],
    ].each do |stmt, expect|
      assert_equal expect, extract_expression(stmt)
    end
  end
end
