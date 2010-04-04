#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'
require_relative '../../app/brkpt'

class TestBreak < Test::Unit::TestCase

  include FnTestHelper

  def setup
    Breakpoint::reset
  end

  def test_break_same_level

    # See that we can stop at a breakpoint
    cmds = ['set basename on',
            'break ' + (__LINE__ + 7).to_s, 
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b1 ###############
    x = 5
    y = 6
    z = 7
    ##############################
    d.stop
    out = ['-- ',
           'x = 5',
           'basename is on.',
           "Breakpoint 1 set at line 26\n" + 
           "\tin file test-break.rb,\n" + 
           "\tVM offset 55 of instruction sequence \"test_break_same_level\".",
           'xx ',
           'z = 7']
    compare_output(out, d, cmds)

    # Try a disabling the breakpoint
    cmds = ['set basename on',
            'break ' + (__LINE__ + 8).to_s, 
            'break ' + (__LINE__ + 8).to_s, 
            'disable 2',
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b2 ###############
    x = 7
    y = 8
    z = 8+1
    ##############################
    d.stop
    out = ['-- ',
           'x = 7',
           'basename is on.',
           "Breakpoint 2 set at line 49\n" +
           "\tin file test-break.rb,\n" + 
           "\tVM offset 55 of instruction sequence \"test_break_same_level\".",
           "Breakpoint 3 set at line 50\n" + 
           "\tin file test-break.rb,\n" + 
           "\tVM offset 55 of instruction sequence \"test_break_same_level\".",
           "Breakpoint 2 disabled.",
           'xx ',
           'z = 8+1']
    compare_output(out, d, cmds)
  end
    
end

