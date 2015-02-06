#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/core'
require_relative 'fn_helper'
require_relative '../../app/breakpoint'

class TestBreakLong < Test::Unit::TestCase

  include FnTestHelper

  def setup
    Trepan::Breakpoint::reset
  end

  def test_break_same_level

    # See that we can stop at a breakpoint
    cmds = ['set basename on',
            'break ' + (__LINE__ + 7).to_s,
            'continue']
    d = strarray_setup(cmds)
    d.start(true)
    ########### b1 ###############
    x = 5
    y = 6
    z = 7
    ##############################
    d.stop
    out = ['line ',
           'x = 5',
           'basename is on.',
 "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_break_same_level\",
\tline 55 in file foo.rb",
           'brkpt ',
           'y = 6']
    compare_output(out, d, cmds)

    # Try a disabling the breakpoint
    cmds = ['set basename on',
            'break ' + (__LINE__ + 8).to_s,
            'break ' + (__LINE__ + 8).to_s,
            'disable 1',
            'continue']
    d = strarray_setup(cmds)
    d.start(true)
    ########### b2 ###############
    x = 7
    y = 8
    z = 8+1
    ##############################
    d.stop
    out = ['line ',
           'x = 7',
           "basename is on.",
           "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_break_same_level\",
\tline 55 in file foo.rb",
           "Breakpoint 2 set at VM offset 55 of instruction sequence \"test_break_same_level\",
\tline 55 in file foo.rb",
           "Breakpoint 1 disabled.",
           'brkpt ',
           'y = 8']
    compare_output(out, d, cmds)

    # Stepping after a breakpoint should not stay at same location.
    cmds = ['set basename on',
            'continue ' + (__LINE__ + 8).to_s,
            'continue']
    dbg = strarray_setup(cmds)
    dbg.start(true)
    ########### b3 ###############
    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    ##############################
    dbg.stop
    out = ['line ',
           'a = 1',
           'basename is on.',
           'brkpt ',
           'c = 3' ]
    compare_output(out, dbg, cmds)
  end

end
