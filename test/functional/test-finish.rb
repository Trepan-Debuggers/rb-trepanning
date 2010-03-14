#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

class TestFinish < Test::Unit::TestCase

  include FnTestHelper

  def test_finish_between_fn
    
    # Finish over functions
    def fact(x)
      return 1 if x <= 1
      x = x * fact(x-1)
      return x
    end
    cmds = %w(step finish) + ['24 == x', 'continue'] 
    d = strarray_setup(cmds)
    d.start
    ##############################
    x = fact(4)
    y = 5
    ##############################
    d.stop # ({:remove => true})
    out = ["-- ",
           "x = fact(4)",
           "METHOD TestFinish#fact(x)",
           "-> ",
           "def fact(x)",
           "<- ",
           "=> 24",
           "end",
           "=> true"]
    compare_output(out, d, cmds)
  end
  
end






