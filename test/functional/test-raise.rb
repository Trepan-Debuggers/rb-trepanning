#!/usr/bin/env ruby
require 'test/unit'
require 'rbconfig'
require_relative 'fn_helper'

class TestRaise < Test::Unit::TestCase

  include FnTestHelper

  def test_raise

    skip "FIXME on mingw" if
      RbConfig::CONFIG['target_os'].start_with?('mingw')
    cmds = [
            'step',
            'step',
            'raise',
            'pr "hi"',
            ]
    d = strarray_setup(cmds)
    d.start(true)
    ##############################
    begin
      x = 1
      y = 2
      z = 3
    rescue RuntimeError
      d.stop
      assert 'We are cool with RuntimeError'
    else
      d.stop
      assert false, 'Should have got RuntimeError'
    end
    ##############################
    out = ['line ', 'begin',
           'line ', 'x = 1',
           'line ', 'y = 2',
           'line ', 'd.stop',
           '"hi"']
    compare_output(out, d, cmds)

    # Try with explicit Exception name
    cmds = [
            'next',
            'raise [5]',
            'raise NotanError',
            'step',
            'raise TypeError',
            ]
    d = strarray_setup(cmds)
    d.start
    ##############################
    begin
      x = 1
      y = 2
      z = 3
    rescue TypeError
      d.stop
      assert 'We are cool with TypeError'
    else
      d.stop
      assert false, 'Should have got TypeError'
    end
    ##############################
    d.stop # ({:remove => true})
    out = ['line ', 'begin',
           'line ', 'x = 1',
           '** "[5]" does not inherit Exception.',
           '** "NotanError" does not inherit Exception.',
           'line ', 'y = 2',
           'line ', 'd.stop']
    compare_output(out, d, cmds)
  end

end
