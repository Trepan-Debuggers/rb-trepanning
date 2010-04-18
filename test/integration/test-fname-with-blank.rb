#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'

class TestFnameWithBlank < Test::Unit::TestCase
  NAME = File.basename(__FILE__, '.rb')[5..-1]

  def test_it
    assert_equal(true, run_debugger(NAME, 'fname with blank.rb'))
  end
end
