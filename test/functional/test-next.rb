#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestNext < Test::Unit::TestCase

    include FnTestHelper

    def test_next_same_level

        # See that we can next with parameter which is the same as 'next 1'
        cmds = %w(next continue)
        d = strarray_setup(cmds)
        d.start(true)
        x = 5
        y = 6
        d.stop
        out = ['line ', 'x = 5', 'line ', 'y = 6']
        compare_output(out, d, cmds)

        # See that we can next with a computed count value
        cmds = ['next 5-3', 'continue']
        d = strarray_setup(cmds)
        d.start(true)
        ########### t1 ###############
        x = 5
        y = 6
        z = 7
        ##############################
        d.stop # ({'remove': true})
        out = ['line ', 'x = 5', 'line ', 'z = 7']
        compare_output(out, d, cmds)
    end

    def test_next_between_fn

        # Next over functions
        cmds = ['next 2', 'continue']
        d = strarray_setup(cmds)
        ########### t2 ###############
        def fact(x)
            return 1 if x <= 1
            return fact(x-1)
        end
        d.start(true)
        x = fact(4)
        y = 5
        ##############################
        d.stop # ({:remove => true})
        out = ['line ', 'x = fact(4)', 'line ', 'y = 5']
        compare_output(out, d, cmds)
    end

    def test_next_in_exception
        cmds = %w(next! continue)
        d = strarray_setup(cmds)
        d.start(true)
        ########### t2 ###############
        begin
            got_boom = false
            x = 4/0
        rescue
            got_boom = true
        end
        ##############################
        d.stop # ({:remove => true})
        out = ['line ',
               'got_boom = false',
               'raise ',
               'ZeroDivisionError: divided by 0',
               'x = 4/0']
        compare_output(out, d, cmds)
    end
end
