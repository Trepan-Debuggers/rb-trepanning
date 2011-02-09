#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../lib/trepanning'

# Test commands completion
class TestLibTrepanning < Test::Unit::TestCase

  def test_completion
    dbgr = Trepan.new
    [
     ['sh', 'sh', ['show']],  # Simple single completion
     ['se', 'se', ['server', 'set']],  # Simple multiple completion
     ['show', 'show', ['show ']], # Add a space because there is more
     ['irb ', 'irb ', []],        # Don't add anything - no more
     # ['irb ', 'irb', []],         # Don't add anything - no more
     ['set au', 'au', ['auto']],  # Single completion when there are two words
     ['set auto', 'auto', ['auto ']], # Add a space because there is more
     ['set auto ', '', ['eval', 'irb', 'list']], # Many two-word completions
     ['set auto e', 'e', ['eval']],
     ['disas', 'disas ', ['disassemble']], # Another single completion
     ['help syn', 'syn', ['syntax']],
     ['help br', 'br', ['break', 'breakpoints']],
     ['set basename o', 'o', ['off', 'on']],
    ].each do |line, token, expect_completion|
      assert_equal(expect_completion, 
                   dbgr.completion_method(token, line),
                   "Bad completion of #{token.inspect} with #{line.inspect}")
    end
    assert(dbgr.completion_method('', '').size > 30, 
           'Initial completion should return more than 30 commands')
  end
end
