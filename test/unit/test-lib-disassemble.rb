#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. lib disassemble)

# Test Debugger:CmdProcessor
class TestLibDisassemble < Test::Unit::TestCase

  include Debugger::Disassemble

  def test_mark_dissassembly
    dis_string='
local table (size: 6, argc: 1 [opts: 0, rest: -1, post: 0, block: -1] s1)
[ 6] relative_feature<Arg>[ 5] c          [ 4] e          [ 3] file       [ 2] absolute_feature
0000 trace            8                                               (  26)
0002 trace            1                                               (  27)
0004 putnil           
'
    [[-1, -1], [1, -1], [2, 4], [10, -1]].each do |num, pos|
      ary = mark_disassembly(dis_string, true, num)
      selected = ary.select{|line| line =~ /\d{4} / && line !~ /^   /}
      if pos == -1
        assert_equal([], selected,
                     'All lines should not be marked')
      else
        assert_equal(1, selected.size,
                     "Should have found exectly one position at #{pos}")
        assert_equal(0, ary[pos] =~ /^--> /,
                     "Should have marked position #{pos}")
      end
    end
  end
end
