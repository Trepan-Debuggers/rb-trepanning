#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/disassemble'

# Test Debugger:CmdProcessor
class TestAppDisassemble < Test::Unit::TestCase

  include Debugger::Disassemble

  def setup
    @dis_string='
local table (size: 6, argc: 1 [opts: 0, rest: -1, post: 0, block: -1] s1)
[ 6] relative_feature<Arg>[ 5] c          [ 4] e          [ 3] file       [ 2] absolute_feature
0000 trace            8                                               (  26)
0002 trace            1                                               (  27)
0004 putnil           
'
  end

  def test_mark_dissassembly
    # Check marking pc offset.
    [[-1, -1], [1, -1], [2, 4], [10, -1]].each do |pc_offset, pos|
      ary = mark_disassembly(@dis_string, true, pc_offset)
      selected = ary.select{|line| line =~ /\d{4} / && line !~ /^    /}
      if pos == -1
        assert_equal([], selected,
                     'All lines should not be marked')
      else
        assert_equal(1, selected.size,
                     "Should have found exectly one PC mark at #{pos}" +
                     "\n%s" % ary.join("\n"))
        assert_equal(0, ary[pos] =~ /^ --> /,
                     "Should have marked PC at position #{pos}" + 
                     "\n%s" % ary.join("\n"))
      end
    end

    # Check marking breakpoints.

    [[2, []], [2, [2]], [10, [0, 4]]].each do |pc_offset, brkpts|
      ary = mark_disassembly(@dis_string, true, pc_offset, brkpts)
      selected = ary.select{|line| line =~ /\d{4} / && line !~ /^ /}
      assert_equal(brkpts.size, selected.size,
                   "Should have found #{brkpts.size} lines " + 
                   "at #{brkpts.join(',')}" + 
                   ("\n%s" % ary.join("\n")))
    end
  end

  def test_disassemble_split
    expect = {
      0=>
      "0000 trace            8                                               (  26)",
      2=>
      "0002 trace            1                                               (  27)",
      4=>"0004 putnil           "
    }
    assert_equal(expect, disassemble_split(@dis_string))
  end
end
