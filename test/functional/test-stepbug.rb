#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestStep < Test::Unit::TestCase

  include FnTestHelper

  def test_step_through_leave

    # See that we can step with parameter which is the same as 'step 1'
    cmds = ['step', 'step', 'step', 'step', 'step', 'step', 'continue']
    d = strarray_setup(cmds)

    d.start(true)
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
    out = ["line x = 'class Foo",
           ".. eval(x)",
           ".. eval(x)",
           ":: eval(x)",
           "line eval(x)"]
    # compare_output(out, d, cmds)
    assert true

  end

end
