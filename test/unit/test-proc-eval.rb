#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor'
require_relative '../../processor/eval'
require_relative '../../app/mock'

# Test Trepan::CmdProcessor Eval portion
class TestProcEval < Test::Unit::TestCase

  def test_basic
    cmdp = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    assert_equal('(eval "x = 1; y = 2")',
                 cmdp.fake_eval_filename('x = 1; y = 2'))
    assert_equal('(eval "x = 1;"...)',
                 cmdp.fake_eval_filename('x = 1; y = 2', 7))
    x = 1
    cmdp.instance_variable_set('@frame', RubyVM::Frame.current)
    cmdp.instance_variable_set('@settings', {:stack_trace_on_error => true})
    assert_equal('1', cmdp.debug_eval('x = "#{x}"'))
    x = 2
    assert_equal('2', cmdp.debug_eval_no_errmsg('x = "#{x}"'))
    assert_equal(nil, cmdp.debug_eval_no_errmsg('x+'))
  end
end
