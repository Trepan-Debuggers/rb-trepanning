#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestRaise < Test::Unit::TestCase

  include FnTestHelper

  def test_return

    cmds = [
            'set events call, return',
            'step',
            'info args',
            'step',
            'info return',
            'set return 10',
            'set events line',
            'step',
            'pr foo_retval',
            ]
    d = strarray_setup(cmds)
    d.start
    ##############################
    x = 1
    def foo(arg)
      5
    end
    foo_retval = foo('ho')
    z = 3
    ##############################
    d.stop
    out = ['-- ',
           'x = 1',
           'Trace events we may stop on:',
           "\tbrkpt, call, return",
           'METHOD TestRaise#foo(arg)',
           '-> ',
           'def foo(arg)',
           "arg = \"ho\"",
           '<- ',
           'R=> 5',
           'end',
           'Return value: 5',
           'Return value was: 5',
           'New value is: 10',
           'Trace events we may stop on:',
           "\tbrkpt, line",
           '-- ',
           'z = 3',
           '10',
           '-- ',
           'd.stop']
    compare_output(out, d, cmds)

    # Try C function
    cmds = [
            'set different off',
            'set events c_call, c_return',
            'step',
            'info args',
            'step',
            'info return',
            'set return "abc"',
            'set events line',
            'step',
            'pr result',
            ]
    d = strarray_setup(cmds)
    d.start
    ##############################
    a = 1
    result = File.basename('/a/b.c')
    ##############################
    d.stop # ({:remove => true})
    out = ["-- ",
           "a = 1",
           'different is off.',
           "Trace events we may stop on:",
           "\tbrkpt, c_call, c_return",
           "CFUNC File#basename(\"/a/b.c\")",
           "C> ",
           "result = File.basename('/a/b.c')",
           "1: \"/a/b.c\"",
           "<C ",
           "R=> \"b.c\"",
           "result = File.basename('/a/b.c')",
           "Return value: \"b.c\"",
           "Return value was: \"b.c\"",
           "New value is: \"abc\"",
           "Trace events we may stop on:",
           "\tbrkpt, line",
           "-- ",
           "d.stop # ({:remove => true})",
           '"abc"']
    compare_output(out, d, cmds)
  end

end
