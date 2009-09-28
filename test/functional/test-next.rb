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
    x = 5
    y = 6
    z = 7
    d.stop # ({'remove': true})
    out = ['-- x = 5',
           '-- z = 7']
    compare_output(out, d, cmds)
  end
    
  def test_next_between_fn
    
    # Next over a function
    def fact(x)
      return 1 if x <= 1
      return fact(x-1)
    end
    cmds = %w(next continue)
    d = strarray_setup(cmds)
    d.start
    x = fact(4)
    y = 5
    d.stop # ({:remove => true})
    out = ['-- x = fact(4)',
           '-- y = 5']
    compare_output(out, d, cmds)
  end
  
  # def test_next_in_exception
  #   def boom(x)
  #     y = 0/x
  #   end
  #   def buggy_fact(x)
  #     return boom(0) if x <= 1
  #     return buggy_fact(x-1)
  #   end
  #   cmds = ['next!', 'continue']
  #   d = strarray_setup(cmds)
  #   begin
  #     d.start
  #     x = buggy_fact(4)
  #     y = 5
  #     assert(false, 'should have raised an exception')
  #   rescue ZeroDivisionError
  #     assert(true, 'Got the exception')
  #   ensure
  #     d.stop # ({:remove => true})
  #   end
    
  #   out = ['-- x = buggy_fact(4)',
  #          '!! x = buggy_fact(4)']
  #   compare_output(out, d, cmds)
  # end
end






