#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'
require_relative %w(.. .. app brkpt)

class TestBreak < Test::Unit::TestCase

  include FnTestHelper

  def setup
    Breakpoint::reset
  end

  def test_condition

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
    out = ['-- x = 6',
           'basename is on.',
           "Breakpoint 1 set at line 26\n" + 
           "\tin file test-condition.rb,\n" + 
           "\tVM offset 55 of instruction sequence test_condition."
          ]
    compare_output(out, d, cmds)

    # Try a condition that fails
    cmds = ['set basename on',
            'break ' + (__LINE__ + 7).to_s, 
            'condition 2 x > 5',
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b2 ###############
    x = 6
    y = 7
    z = 8
    ##############################
    d.stop
    out = ['-- x = 6',
           'basename is on.',
           "Breakpoint 2 set at line 47\n" +
           "\tin file test-condition.rb,\n" + 
           "\tVM offset 55 of instruction sequence test_condition.",
           'xx y = 7']
    compare_output(out, d, cmds)
  end
    
end

