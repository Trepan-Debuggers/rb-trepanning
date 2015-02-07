#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestEval < Test::Unit::TestCase

    include FnTestHelper

    def test_eval_questionmark

        # See that eval? strips 'if'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        if 3 > 5
            assert false
        end
        d.stop
        out = ['line ', 'if 3 > 5', 'eval: 3 > 5', 'false']
        compare_output(out, d, cmds)

        # See that eval? strips 'if' and 'then'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        if 3 > 5 then
            assert false
        end
        d.stop
        out = ['line ', 'if 3 > 5 then', 'eval: 3 > 5', 'false']
        compare_output(out, d, cmds)

        # See that eval? strips 'unless'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        unless 3 < 5
            assert false
        end
        d.stop
        out = ['line ', 'unless 3 < 5', 'eval: 3 < 5', 'true']
        compare_output(out, d, cmds)

        # See that eval? strips 'unless' and 'then
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        unless 3 < 5 then
            assert false
        end
        d.stop
        out = ['line ', 'unless 3 < 5 then', 'eval: 3 < 5', 'true']
        compare_output(out, d, cmds)

        # See that eval? strips 'while'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        while nil
            assert false
        end
        d.stop
        out = ['line ', 'while nil', 'eval: nil', 'nil']
        compare_output(out, d, cmds)

        # See that eval? strips 'while' and 'do'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        while nil do
            assert false
        end
        d.stop
        out = ['line ', 'while nil do', 'eval: nil', 'nil']
        compare_output(out, d, cmds)

        # See that eval? strips 'until' and 'do'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        until true do
            assert false
        end
        d.stop
        out = ['line ', 'until true do', 'eval: true', 'true']
        compare_output(out, d, cmds)

        # See that eval? strips 'until'
        cmds = %w(eval? continue)
        d = strarray_setup(cmds)
        d.start(true)
        until true
            assert false
        end
        d.stop
        out = ['line ', 'until true', 'eval: true', 'true']
        compare_output(out, d, cmds)

        # FIXME
        # # See that eval? strips 'return'
        # def five
        #   return 5
        # end
        # cmds = %w(step step eval? continue)
        # d = strarray_setup(cmds)
        # d.start(true)
        # five
        # d.stop
        # out = ['line ', 'five',  'call', 'return 5', 'return',
        #        '-> ', 'def five', 'line ', 'return 5', 'eval: 5', '5']
        # compare_output(out, d, cmds)
    end
end
