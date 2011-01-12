#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'

class TestQuit < Test::Unit::TestCase
  @@NAME = File.basename(__FILE__, '.rb')[5..-1]

  def test_trepan_call
    opts = {}
    opts[:filter] = Proc.new{|got_lines, correct_lines|
      got_lines[0] = "-> (null.rb:1 @0)\n"
    }
    assert_equal(true, run_debugger(@@NAME, 'null.rb', opts))
  end
end
