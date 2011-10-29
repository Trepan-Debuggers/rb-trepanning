#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

class TestRecursiveBt < Test::Unit::TestCase

  include FnTestHelper

  def test_recursive_backtrace

    cmds = [
            'set events line',
            'set basename on',
            'step',
            'bt 1',
            'step',
            'step',
            'bt 2',
            'step',
            'step',
            'bt 3',
            'step',
            'step',
            'step',
            'bt 5',
            'step',
            'step',
            'step',
            'bt 7',
            ]
    d = strarray_setup(cmds)
    d.start
    ##############################
    def factorial(n)
      if n > 0
        return n * factorial(n-1)
      else
        return 1
      end
    end
    z = factorial(5)
    ##############################
    d.stop
    out = 
      ["-- ",
       "def factorial(n)",
       "Trace events we may stop on:",
       "\tbrkpt, line",
       "basename is on.",
       "-- ",
       "z = factorial(5)",
       "--> #0 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "-- ",
       "if n > 0",
       "-- ",
       "return n * factorial(n-1)",
       "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #1 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "-- ",
       "if n > 0",
       "-- ",
       "return n * factorial(n-1)",
       "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #2 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "-- ",
       "if n > 0",
       "-- ",
       "return n * factorial(n-1)",
       "-- ",
       "if n > 0",
       "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 36",
       "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "... above line repeated 2 times",
       "    #4 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "-- ",
       "return n * factorial(n-1)",
       "-- ",
       "if n > 0",
       "-- ",
       "return n * factorial(n-1)",
       "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "... above line repeated 3 times",
       "    #5 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "    #6 METHOD TestRecursiveBt#run(runner) in file unit.rb at line 695",
       "(More stack frames follow...)",
       "-- ",
       "if n > 0",
       "-- ",
       "return 1",
       "-- ",
       "d.stop"]
    compare_output(out, d, cmds)

  end
end
