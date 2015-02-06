#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'

class TestQuit < Test::Unit::TestCase
  @@NAME = File.basename(__FILE__, '.rb')[5..-1]

  # def test_trepanx_set_confirm_off
  #   opts = {}
  #   opts[:filter] = Proc.new{|got_lines, correct_lines|
  #     got_lines[0] = "-> (null.rb:1 @0)\n"
  #   }
  #   assert_equal(true, run_debugger('quit2', 'null.rb', opts))
  # end

  def test_trepan_pc
      skip "Can't run from rake :-(" if __FILE__ != $0
      opts = {}
      opts[:filter] = Proc.new{|got_lines, correct_lines|
          got_lines[0] = "-> (null.rb:1 @0)\n"
      }
      assert_equal(true, run_debugger(@@NAME, 'assign.rb', opts))
  end
end
