#!/usr/bin/env ruby
require 'test/unit'
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
    d.start(true)
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
      ["line ",
       "def factorial(n)",
       "Trace events we may stop on:",
       "\tbrkpt, line",
       "basename is on.",
       "line ",
       "z = factorial(5)",
       "line> #0 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "line ",
       "if n > 0",
       "line ",
       "return n * factorial(n-1)",
       "line> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #1 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "line ",
       "if n > 0",
       "line ",
       "return n * factorial(n-1)",
       "line> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #2 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "line ",
       "if n > 0",
       "line ",
       "return n * factorial(n-1)",
       "line ",
       "if n > 0",
       "line> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 36",
       "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "... above line repeated 2 times",
       "    #4 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "(More stack frames follow...)",
       "line ",
       "return n * factorial(n-1)",
       "line ",
       "if n > 0",
       "line ",
       "return n * factorial(n-1)",
       "line> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
       "... above line repeated 3 times",
       "    #5 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 42",
       "    #6 METHOD TestRecursiveBt#run(runner) in file unit.rb at line xxx",
       "(More stack frames follow...)",
       "line ",
       "if n > 0",
       "line ",
       "return 1",
       "line ",
       "d.stop"]
    d.intf[-1].output.output[-8].sub!(/at line \d+$/, 'at line xxx')
    compare_output(out, d, cmds)

  end
end
