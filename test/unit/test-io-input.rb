#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../io/input'

# Test Debugger:CmdProcessor
class TestIOInput < Test::Unit::TestCase

  def test_DebuggerInput
    inp = Debugger::UserInput.open(__FILE__)
    assert inp, 'Should have gotten a DebuggerInput object back'
    line = inp.readline.chomp
    assert_equal '#!/usr/bin/env ruby', line
    assert_equal false, inp.eof?
    inp.close
    assert_equal true, inp.closed?
    inp = Debugger::UserInput.open(__FILE__)
    while not inp.eof?
      begin
        inp.readline
      rescue EOFError
        assert_equal(true, inp.eof?, 
                     'EOF should be true after EOFError')
      end
    end
  end
end
