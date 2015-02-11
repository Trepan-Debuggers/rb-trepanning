#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestRecursiveBt < Test::Unit::TestCase

  include FnTestHelper

  def test_recursive_backtrace

      cmds = [
              'set max width 300',
              'set different off',
              'set events line',
              'step',
              'bt 2',
              'step',
              'step',
              'bt 3',
              'step',
              'step',
              'step',
              'bt 4',
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
    ##############################
    def factorial(n)
      if n > 0
        return n * factorial(n-1)
      else
        return 1
      end
    end
    d.start(true)
    z = factorial(5)
    ##############################
    d.stop
    out =
        ["line ",
         "z = factorial(5)",
         "max width is 300.",
         "different is off.",
         "Trace events we may stop on:\n----------------------------",
         "  line",
         "line ",
         "if n > 0",
         "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 36",
         "    #1 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 43",
         "(More stack frames follow...)",
         "line ",
         "return n * factorial(n-1)",
         "line ",
         "if n > 0",
         "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 36",
         "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
         "    #2 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 43",
         "(More stack frames follow...)",
         "line ",
         "return n * factorial(n-1)",
         "line ",
         "if n > 0",
         "line ",
         "return n * factorial(n-1)",
         "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
         "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
         "... above line repeated 1 times",
         "    #3 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 43",
         "(More stack frames follow...)",
         "line ",
         "if n > 0",
         "line ",
         "return n * factorial(n-1)",
         "line ",
         "if n > 0",
         "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 36",
         "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
         "(More stack frames follow...)",
         "line ",
         "return n * factorial(n-1)",
         "line ",
         "if n > 0",
         "line ",
         "return 1",
         "--> #0 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 39",
         "    #1 METHOD TestRecursiveBt#factorial(n) in file test-recursive-bt.rb at line 37",
         "... above line repeated 4 times",
         "    #6 METHOD TestRecursiveBt#test_recursive_backtrace() in file test-recursive-bt.rb at line 43",
         "(More stack frames follow...)",
         "line ",
         "d.stop",
         "line ",
         "RubyVM::Frame::get.trace_off = true"]

    d.intf[-1].output.output.each do |line|
        line.sub!(/in file .*test-recursive-bt.rb/, 'in file test-recursive-bt.rb')
    end
    compare_output(out, d, cmds)

  end
end
