#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

# See that we hande "set trace var" properly
class TestTraceVar < Test::Unit::TestCase

  include FnTestHelper
  include Trace

  def test_basic

    cmds = ['set trace var $my_var', 'continue', 'continue', 'continue']
    d = strarray_setup(cmds)
    d.core.step_events = TEST_STEP_EVENT_MASK

    d.start
    ########### t1 ###############
    x = 1
    $my_var = 5
    y = 2
    $my_var = 6
    z = 3
    ##############################
    d.stop
    out = ['-- ',
           'x = 1',
           'Tracing variable $my_var.',
           '$V ',
           '$my_var = 5',
           'Note: we are stopped *after* the above location.',
           '$V ',
           '$my_var = 6',
           'Note: we are stopped *after* the above location.',
          ]
    compare_output(out, d, cmds)
    end

end
