#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'

class TestFnameWithBlank < Test::Unit::TestCase
  @@NAME = File.basename(__FILE__, '.rb')[5..-1]

  def test_it
    opts = {}
    opts[:filter] = Proc.new{|got_lines, correct_lines|
      got_lines[0] = "-> (fname with blank.rb:1 @0)\n"
    }
    assert_equal(true, run_debugger(@@NAME, 'fname with blank.rb', opts))
  end
end
