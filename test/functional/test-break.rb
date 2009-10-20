#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

class TestBreak < Test::Unit::TestCase

  include FnTestHelper

  def test_break_same_level

    # See that we can next with parameter which is the same as 'next 1'
    cmds = ['set basename on',
            'break ' + (__LINE__ + 7).to_s, 
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b1 ###############
    x = 5
    y = 6
    z = 10
    ##############################
    d.stop
    out = ['-- x = 5',
           'basename is on.',
           "Breakpoint 1 set in file test-break.rb,\n" + 
           "\tVM offset 55 of instruction sequence test_break_same_level.",
           'xx z = 10']
    compare_output(out, d, cmds)

    # Try a disable command
    cmds = ['set basename on',
            'break ' + (__LINE__ + 8).to_s, 
            'break ' + (__LINE__ + 8).to_s, 
            'disable 2',
            'continue']
    d = strarray_setup(cmds)
    d.start
    ########### b1 ###############
    x = 5
    y = 6
    z = 10
    ##############################
    d.stop
    out = ['-- x = 5',
           'basename is on.',
           "Breakpoint 2 set in file test-break.rb,\n" + 
           "\tVM offset 55 of instruction sequence test_break_same_level.",
           "Breakpoint 3 set in file test-break.rb,\n" + 
           "\tVM offset 55 of instruction sequence test_break_same_level.",
           'xx z = 10']
    compare_output(out, d, cmds)
  end
    
end

