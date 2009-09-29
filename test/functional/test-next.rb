#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

class TestNext < Test::Unit::TestCase

  include FnTestHelper

  def test_next_same_level

    # See that we can next with parameter which is the same as 'next 1'
    cmds = %w(next continue)
    d = strarray_setup(cmds)
    d.start
    x = 5
    y = 6
    d.stop
    out = ['-- x = 5',
           '-- y = 6']
    compare_output(out, d, cmds)
    
    # See that we can next with a computed count value
    cmds = ['next 5-3', 'continue']
    d = strarray_setup(cmds)
    d.start
    ########### t1 ###############
    x = 5
    y = 6
    z = 7
    ##############################
    d.stop # ({'remove': true})
    out = ['-- x = 5',
           '-- z = 7']
    compare_output(out, d, cmds)
  end
    
  def test_next_between_fn
    
    # Next over functions
    cmds = ['next 2', 'continue']
    d = strarray_setup(cmds)
    d.start
    ########### t2 ###############
    def fact(x)
      return 1 if x <= 1
      return fact(x-1)
    end
    x = fact(4)
    y = 5
    ##############################
    d.stop # ({:remove => true})
    out = ['-- def fact(x)',
           '-- y = 5']
    compare_output(out, d, cmds)
  end
  
#   def test_next_in_exception
#     cmds = ['next! 4', 'continue']
#     d = strarray_setup(cmds)
#     d.start
#     ########### t2 ###############
#     def boom(x)
#       y = 0/x
#     end
#     begin
#       got_boom = false
#       x = boom(4)
#     rescue
#       bot_boom = true
#     end
#     ##############################
#     d.stop # ({:remove => true})
#     out = ['-- x = buggy_fact(4)',
#            '!! x = buggy_fact(4)']
#     compare_output(out, d, cmds)
#   end
end






