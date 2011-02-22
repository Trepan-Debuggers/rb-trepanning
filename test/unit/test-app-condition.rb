#!/usr/bin/env ruby
require 'test/unit'
require 'stringio'
require_relative '../../app/condition'

class TestAppCondition < Test::Unit::TestCase
  include Trepan::Condition
  
  def test_basic
    assert valid_condition?('1+2')
    old_stderr = $stderr
    new_stdout = StringIO.new
    $stderr = new_stdout
    assert_equal(nil, valid_condition?('1+'), 
                 "Should have not been able to parse 1+")
    $stderr = old_stderr
    assert_equal(false, new_stdout.string.empty?, 
                 "Should have gotten some sort of compile error on stderr")
  end
end
