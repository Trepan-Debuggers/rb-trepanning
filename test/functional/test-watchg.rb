#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

# See that we hande "set trace var" properly
class TestWatchG < Test::Unit::TestCase

    include FnTestHelper

    def test_basic

        cmds = ['watchg $my_var',
                'continue', 'continue']
        d = strarray_setup(cmds)

        d.start(true)
        ########### t1 ###############
        x = 1
        $my_var = 5
        y = 2
        $my_var = 6
        z = 3
        ##############################
        d.stop

        out = ["line ",
               "x = 1",
               "Tracing for variable $my_var set to: stop.",
               "trace: $my_var = 5",
               "trace-var ",
               "y = 2",
               "trace: $my_var = 6",
               "trace-var ",
               "z = 3",
               "c_return ",
               "R=> nil",
               "z = 3",
               "line ",
               "z = 3",
               "line ",
               "d.stop",
               "call ",
               # FIXME: can we get rid of the below?
               "RubyVM::Frame::get.trace_off = true",
               "c_call ",
               "RubyVM::Frame::get.trace_off = true",
               "c_call ",
               "RubyVM::Frame::get.trace_off = true"
              ]
        compare_output(out, d, cmds)
    end
end
