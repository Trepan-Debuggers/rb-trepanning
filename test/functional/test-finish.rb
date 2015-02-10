#!/usr/bin/env ruby
require 'test/unit'
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
      d.start(true)
      ##############################
      x = fact(4)
      y = 5
      ##############################
      d.stop # ({:remove => true})
      out = ['line ',
             'x = fact(4)',
             'call ',
             'return 1 if x <= 1',
             'return ',
             'R=> 24',
             'end',
             'D=> true']
      compare_output(out, d, cmds)
  end

end
