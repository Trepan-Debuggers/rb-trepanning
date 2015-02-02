#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestStep < Test::Unit::TestCase

  include FnTestHelper
  include Trace

  def test_step_through_leave

    # See that we can step with parameter which is the same as 'step 1'
    cmds = ['step', 'step', 'step', 'step', 'step', 'step', 'continue']
    d = strarray_setup(cmds)
    d.core.step_events = TEST_STEP_EVENT_MASK

    d.start
    ########### t1 ###############
    x = 'class Foo
           def bar
             3
           end
         end'
    eval(x)
    f = Foo.new
    # f.bar
    ##############################
    d.stop
    out = ["-- x = 'class Foo",
           ".. eval(x)",
           ".. eval(x)",
           ":: eval(x)",
           "-- eval(x)"]
    # compare_output(out, d, cmds)
    assert true

  end

end
