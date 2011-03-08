#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'
require_relative '../../app/breakpoint'

class TestDelete < Test::Unit::TestCase

  include FnTestHelper

  def test_delete

    # See that when we can delete a breakpoint.
    cmds = ['set basename on',
            'break ' + (__LINE__ + 9).to_s, 
            'delete 1', 
            'continue',
           ]
    d = strarray_setup(cmds)
    d.start
    ########### b1 ###############
    va = 1
    vb = 2
    vc = 3
    vd = 4
    ve = 5
    ##############################
    d.stop
    out = ['-- ',
           'va = 1',
           'basename is on.',
           "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_delete\",
\tline 55 in file foo.rb",
           "Deleted breakpoint 1."]
    compare_output(out, d, cmds)

    # See that when we can delete a breakpoint but get to the next one.
    cmds = ['set basename on',
            'set autoeval on',
            'break ' + (__LINE__ + 11).to_s, 
            'break ' + (__LINE__ + 12).to_s, 
            'delete 1', 
            'continue',
            'va',
            'continue',
           ]
    d = strarray_setup(cmds)
    d.start
    ########### b1 ###############
    va = 1
    vb = 2
    va = 3
    vd = 4
    ve = 5
    ##############################
    d.stop
    out = ["-- ",
           "va = 1",
           "basename is on.",
           "Evaluation of unrecognized debugger commands is on.",
           "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_delete\",
\tline 55 in file foo.rb",
           "Breakpoint 2 set at VM offset 55 of instruction sequence \"test_delete\",
\tline 55 in file foo.rb",
           "Deleted breakpoint 1.",
           "xx ",
           "vd = 4",
           "D=> 3"]
    compare_output(out, d, cmds)

  end
    
end

