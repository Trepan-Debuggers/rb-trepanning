#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestBreak < Test::Unit::TestCase

  include FnTestHelper

  def test_line_only_break
    # Check that we can set breakpoints in parent, sibling and children
    # of sibling returns. We have one more 'continue' than we need
    # just in case something goes wrong.
    cmds_pat = ((['break %d'] * 3) + (%w(continue) * 4)).join("\n")
    line = __LINE__
    cmds = (cmds_pat % [line, line+11, line+14]).split(/\n/)
    d = strarray_setup(cmds)
    ##############################
    def foo      # line +  4  
      a = 5      # line +  5
      b = 6      # line +  6
    end          # line +  7
    1.times do   # line +  8 
      d.start    # line +  9
      1.times do # line + 10
        x = 11   # line + 11
        foo      # line + 12
      end        # line + 13
      c = 14     # line + 14
    end
    ##############################
    d.stop # ({:remove => true})
    out = ["-- ",
           "1.times do # line + 10",
           "Breakpoint 1 set at VM offset 55 of instruction sequence \"test_line_only_break\",
\tline 55 in file foo.rb",
           "Breakpoint 2 set at VM offset 55 of instruction sequence \"block (2 levels) in test_line_only_break\",
\tline 55 in file foo.rb",
           "Breakpoint 3 set at VM offset 55 of instruction sequence \"block in test_line_only_break\",
\tline 55 in file foo.rb",
           "xx ",
           "x = 11   # line + 11",
           "xx ",
           "c = 14     # line + 14"]
    compare_output(out, d, cmds)
  end

end


