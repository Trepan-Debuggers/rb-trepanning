# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
# Splits String dissasemble_str into an array for for each
# line. Line(s) that Fixnum pc_offset matches at the beginning of the
# line will have prefix "--> " added, otherwise the line will have " "
# added to the beginning.  This array is returned.

class Trepan
  module Disassemble
    def mark_disassembly(disassembly_str, iseq_equal, pc_offset,
                         brkpt_offsets=[], max_width=80)
      dis_array = disassembly_str.split(/\n/)
      dis_array.map do |line|
        if line =~ /^(.*?)(\s+)(\(\s+\d+\))?$/
          line_begin   = $1
          line_padding = $2
          line_end     = $3 || ''
        else
          line_begin   = line
          line_padding = ''
          line_end     = ''
        end
          
        prefix = 
          if line =~ /^(\d{4}) /
            offset = $1.to_i
            bp = brkpt_offsets && brkpt_offsets.member?(offset) ? 'B' : ' '
            bp + if offset == pc_offset && iseq_equal
              '--> '
            else
              '    '
            end
          else
            ''
          end
        line_size = prefix.size + line.size
        shrink_amount = line_size - max_width
        if (shrink_amount > 0) && line_padding.size > shrink_amount
          line_padding = ' ' * (line_padding.size - shrink_amount)
        end
        prefix + line_begin + line_padding + line_end
      end
    end
    module_function :mark_disassembly

    def disassemble_split(disassembly_str)
      dis_array = disassembly_str.split(/\n/)
      dis_hash  = {}
      dis_array.each do |line|
        dis_hash[$1.to_i] = line if line =~ /^(\d{4}) /
      end
      dis_hash
    end
    module_function :disassemble_split

  end
end

if __FILE__ == $0
  # Demo it.
  include Trepan::Disassemble
  dis_string='
local table (size: 6, argc: 1 [opts: 0, rest: -1, post: 0, block: -1] s1)
[ 6] relative_feature<Arg>[ 5] c          [ 4] e          [ 3] file       [ 2] absolute_feature
0000 trace            8                                               (  26)
0002 trace            1                                               (  27)
0004 putnil           
'
  [[-1, []], [2, [2]], [10, [0, 4]]].each do |pc_offset, brkpts|
    puts '-' * 40
    puts mark_disassembly(dis_string, true, pc_offset, brkpts).join("\n")
  end
  puts mark_disassembly(dis_string, false, 2).join("\n")
  puts '=' * 40
  # FIXME:
  # require_relative '../lib/trepanning'
  # debugger
  # There is a bug in: "breakpoint mark_disassembly" and 
  # breakpoint mark_disassembly o10"
  str = mark_disassembly(dis_string, false, 2, [], 70).join("\n")
  puts str
  puts '=' * 40
  require 'pp'
  PP.pp(disassemble_split(dis_string), $stdout)
end
