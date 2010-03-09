#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. processor main) # Have to include before frame!
                                          # FIXME
require_relative %w(.. .. processor eval)
require_relative %w(.. .. app mock)

# Test Debugger:CmdProcessor Eval portion
class TestProcEval < Test::Unit::TestCase

  def test_basic
    cmdp = Debugger::CmdProcessor.new(Debugger::MockCore.new())
    assert_equal('(eval "x = 1; y = 2")',
                 cmdp.fake_eval_filename('x = 1; y = 2'))
    assert_equal('(eval "x = 1;"...)',
                 cmdp.fake_eval_filename('x = 1; y = 2', 7))
    x = 1
    require 'thread_frame'
    cmdp.instance_variable_set('@frame', RubyVM::ThreadFrame.current)
    cmdp.instance_variable_set('@settings', {:stack_trace_on_error => true})
    assert_equal('1', cmdp.debug_eval('x = "#{x}"'))
    x = 2
    assert_equal('2', cmdp.debug_eval_no_errmsg('x = "#{x}"'))
    assert_equal(nil, cmdp.debug_eval_no_errmsg('x+'))
  end
end
