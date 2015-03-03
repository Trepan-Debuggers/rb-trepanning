#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestRaise < Test::Unit::TestCase

  include FnTestHelper

  def test_return

    cmds = [
            'set max width 80',
            'set different off',
            'set events call, return',
            'step-',
            'info args',
            'step-',
            'info return',
            'set return 10',
            'set events line',
            'step',
            'pr foo_retval',
            ]
    d = strarray_setup(cmds)
    ##############################
    def foo(arg)
      5
    end
    d.start(true)
    x = 1
    foo_retval = foo('ho')
    z = 3
    ##############################
    d.stop
    out = ['line ',
           'x = 1',
           'max width is 80.',
           'different is off.',
           "Trace events we may stop on:\n----------------------------",
           '  call    return',
           'call ',
           '5',
           "arg = \"ho\"",
           "Values may have change from the initial call values.",
           'return ',
           'R=> 5',
           'end',
           'Return class: Fixnum',
           'Return value: 5',
           'Old value was: 5',
           'New value is: 10',
           "Trace events we may stop on:\n----------------------------",
           '  line',
           'line ',
           'z = 3',
           '10',
           'line ',
           'd.stop',
           "line ",
           'RubyVM::Frame::get.trace_off = true'
          ]
    compare_output(out, d, cmds)

    # Try a C function
    cmds = [
            'set max width 80',
            'set different off',
            'set events c_call, c_return',
            'step',
            'info args',
            'step',
            'set events line',
            'step',
            'pr result',
            ]
    d = strarray_setup(cmds)
    d.start(true)
    ##############################
    a = 1
    result = File.basename('/a/b.c')
    ##############################
    d.stop
    out = ["line ",
           "a = 1",
           'max width is 80.',
           'different is off.',
           "Trace events we may stop on:\n----------------------------",
           "  c_call    c_return",
           "c_call ",
           "result = File.basename('/a/b.c')",
           "1: \"/a/b.c\"",
           "c_return ",
           "R=> \"b.c\"",
           "result = File.basename('/a/b.c')",
           "Trace events we may stop on:\n----------------------------",
           "  line",
           "line ",
           'd.stop',
           "\"b.c\"",
           "line ",
           "RubyVM::Frame::get.trace_off = true",
          ]
    compare_output(out, d, cmds)
  end

end
