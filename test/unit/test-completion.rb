#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../lib/trepanning'

# We don't want to do completion or save history.
# This is one hacky way to make sure this doesn't happen
def Trepan::GNU_readline?
  false
end

# Test commands completion
class TestCompletion < Test::Unit::TestCase
  def test_completion
    dbgr = Trepan.new
    [
     ['sho', 'sho', ['show']],  # Simple single completion
     ['se', 'se', ['server', 'set']],  # Simple multiple completion
     ['show', 'show', ['show']], # Completion when word is complete
     ['irb ', 'irb ', []],        # Don't add anything - no more
     ['set auto', 'auto', ['auto']], # Single completion on two words
     ['set au', 'au', ['auto']],  # Single completion when there are two words
     ['sho aut', 'aut', ['auto']], # Add a space because there is more
     ['set auto eval ', '', ['off', 'on']], # Many 3-word completions
     ['set auto ', '', ['eval', 'irb', 'list']], # Many two-word completions
     ['set auto e', 'e', ['eval']],
     ['disas', 'disas', ['disassemble']], # Another single completion
     ['help syn', 'syn', ['syntax']],
     ## FIXME:
     ## ['help syntax', 'co', ['command']],
     ['help br', 'br', ['break', 'breakpoints']],
     ['where', 'where', ['where']],  # Single alias completion
     ['set basename o', 'o', ['off', 'on']],
    ].each do |line, token, expect_completion|
      # require_relative '../../lib/trepanning'
      # debugger if line == 'help syntax co'
      assert_equal(expect_completion,
                   dbgr.completion_method(token, line),
                   "Bad completion on #{line.inspect} with #{token.inspect}")
    end
    assert(dbgr.completion_method('', '').size > 30,
           'Initial completion should return more than 30 commands')
  end
end
