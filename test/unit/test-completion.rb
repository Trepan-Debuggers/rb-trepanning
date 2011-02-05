#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../lib/trepanning'

# Test commands completion
class TestLibTrepanning < Test::Unit::TestCase

  def test_completion
    dbgr = Trepan.new
    [['sh', 'sh', ['show']],
     ['se', 'se', ['server', 'set']],
     ['show', 'show', ['show ']],
     ['set au', 'au', ['auto']],
     ['set auto', 'auto', ['auto ']],
     ['set auto e', 'e', ['eval']],
     ['help syn', 'syn', ['syntax']],
     ['help br', 'br', ['break', 'breakpoints']],
     ['set basename o', 'o', ['off', 'on']],
    ].each do |line, token, expect_completion|
      assert_equal(expect_completion, 
                   dbgr.completion_method(token, line),
                   "Bad completion of #{token.inspect} with #{line.inspect}")
    end
  end
end
