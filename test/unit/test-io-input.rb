#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. io input)

# Test Debugger:CmdProcessor
class TestIOInput < Test::Unit::TestCase

  def test_DebuggerInput
    inp = Debugger::UserInput.open(__FILE__)
    assert inp, 'Should have gotten a DebuggerInput object back'
    line = inp.readline.chomp
    assert_equal '#!/usr/bin/env ruby', line
    inp.close
    return
  end
end
