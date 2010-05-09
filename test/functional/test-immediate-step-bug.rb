#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

class TestStep < Test::Unit::TestCase

  include FnTestHelper
  include Trace

  def test_step_same_level

    # See that we can step with parameter which is the same as 'step 1'
    cmds = ['next', 'next', 'continue', 'continue']
    d = strarray_setup(cmds)
    d.core.step_events = TEST_STEP_EVENT_MASK

    ########### immediate bug1 ###############
    2.times do 
      x = 1
      d.debugger(:immediate => true)
      y = 2
    end
    ##############################
    d.stop
    out = [':o ', 'd.debugger(:immediate => true)', 
           '-- ', 'y = 2',
           '-- ', 'x = 1',
           ':o ', 'd.debugger(:immediate => true)']
    compare_output(out, d, cmds)
  end
end
