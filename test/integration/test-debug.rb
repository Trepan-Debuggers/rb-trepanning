#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'
require 'rbconfig'

class TestDebug < Test::Unit::TestCase
  @@NAME = File.basename(__FILE__, '.rb')[5..-1]
    def test_trepan_debug
        opts = {:args => '3 5', :verbose=>true}
        assert_equal(true, run_debugger(@@NAME, 'gcd.rb', opts))
    end
end
