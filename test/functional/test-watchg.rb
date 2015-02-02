#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

# See that we hande "set trace var" properly
class TestWatchG < Test::Unit::TestCase

  include FnTestHelper
  include Trace

  def test_basic

    cmds = ['watchg $my_var',
            'watchg $my_var',
            'continue', 'continue',
            'watchg $my_var off',
            'watchg $my_var off',
            'continue']
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
           'Tracing for variable $my_var set to: stop.',
           '** global variable $my_var is already traced with stop.',
           'trace: $my_var = 5',
           'Note: we are stopped *after* the above location.',
           '$V ',
           '$my_var = 5',
           'trace: $my_var = 6',
           'Note: we are stopped *after* the above location.',
           '$V ',
           '$my_var = 6',
           'Removed trace for variable $my_var.',
           'Warning: variable $my_var is not currently marked as traced.',
           'Removed trace for variable $my_var.'
          ]
    compare_output(out, d, cmds)
    end

end
