# Splits String dissasemble_str into an array for for each
# line. Line(s) that Fixnum pc_offset matches at the beginning of the
# line will have prefix "--> " added, otherwise the line will have " "
# added to the beginning.  This array is returned.

class Debugger
  module Disassemble
    # FIXME: add a routine to split into separate groups of instruction
    # sequences and access by offset.
    def mark_disassembly(disassembly_str, pc_offset)
      dis_array = disassembly_str.split(/\n/)
      offset_str = "%04d " % pc_offset
      dis_array.map do |line|
        # FIXME: offset is not enough, we also need the
        # instruction sequence *name*.
        prefix = if line =~ /\d{4} /
                   (line =~ /^#{offset_str}/ ?  '--> ': '    ')
                 else
                   ''
                 end
        prefix + line
      end
    end
    module_function :mark_disassembly
  end
end

if __FILE__ == $0
  # Demo it.
  include Debugger::Disassemble
  dis_string='
local table (size: 6, argc: 1 [opts: 0, rest: -1, post: 0, block: -1] s1)
[ 6] relative_feature<Arg>[ 5] c          [ 4] e          [ 3] file       [ 2] absolute_feature
0000 trace            8                                               (  26)
0002 trace            1                                               (  27)
0004 putnil           
'
  [-1, 2, 10].each do |num|
    puts '-' * 20
    puts mark_disassembly(dis_string, num).join("\n")
  end
end
