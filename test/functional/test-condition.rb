#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'
require_relative '../../app/breakpoint'

class TestBreak < Test::Unit::TestCase

  include FnTestHelper

  def test_condition
    file = File.basename(__FILE__)

    # See that we can next with parameter which is the same as 'next 1'
    cmds = ['set basename on',
            'break ' + (__LINE__ + 7).to_s,
            'condition 1 x < 5',
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b1 ###############
    x = 6
    y = 7
    z = 8
    ##############################
    d.stop
    out = ['-- ',
           'x = 6',
           'basename is on.',
           "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_condition\",
\tline 55 in file foo.rb"]
    compare_output(out, d, cmds)

    # Try a condition that fails
    cmds = ['set basename on',
            'break ' + (__LINE__ + 7).to_s,
            'condition 1 x > 5',
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b2 ###############
    x = 6
    y = 7
    z = 8
    ##############################
    d.stop
    out = ["-- ",
           "x = 6",
           "basename is on.",
           "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_condition\",
\tline 55 in file foo.rb",
           "xx ",
           "y = 7"]
    compare_output(out, d, cmds)
  end

end
