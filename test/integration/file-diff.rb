require 'diff/lcs'

# Consider turning this into 'diff/lcs/file' or some such thing.

module DiffFile
  def find_next(sdiff, i)
    i += 1 while i < sdiff.size && sdiff[i].action == '='
    return i
  end
  
  def find_pos(sdiff, i)
    i += 1 while i < sdiff.size && sdiff[i].action != '='
    return sdiff[i].old_position, sdiff[i].new_position
  end

  # Unix style context diff using files. 
  def diff_file(from_file, to_file, context=3)
    seq1       = File.open(from_file).readlines
    from_mtime = File.stat(from_file).mtime
    seq2       = File.open(to_file).readlines
    to_mtime   = File.stat(to_file).mtime
    sdiff = Diff::LCS.sdiff(seq1, seq2)
    diff_lines(seq1, seq2, from_file, to_file, from_mtime, 
               to_mtime, context)
  end

  # Unix style context diff with file/line stat info passed in.

  # There may be some imprecision in position stuff here and there's a
  # bug when context ranges overlap.

  # FIXME: consider turning into an enumerator.
  def diff_lines(seq1, seq2, from_file, to_file, 
                 from_mtime, to_mtime, context=3)
    sdiff = Diff::LCS.sdiff(seq1, seq2)
    # PP.pp sdiff
    # puts '-' * 40
    started = false
    result = []
    n = i = 0
    while true do
      i = find_next(sdiff, i)
      break if i >= sdiff.size
      if n >= i
        if started
          result << '***************'
        else
          result << "*** #{from_file}\t#{from_mtime}"
          result << "--- #{to_file}\t#{to_mtime}"
          started = true
        end
        result << '*** %d,%d ****' % find_pos(sdiff, i)
        context.downto(1) do |j|
          result << ' ' + sdiff[i-j].new_element.chomp
        end
      end
      i += 1
      while i < sdiff.size && (action = sdiff[i-1].action) != '='
        element = 
          if action == '+'  
            sdiff[i-1].new_element
          else
            sdiff[i-1].old_element
          end
        result << action + element.chomp
        i += 1
      end
      -1.upto(context-2) do |j|
        n = i+j
        break if sdiff[n].action != '='
        result << ' ' + sdiff[n].new_element.chomp
      end
    end
    return result
  end
end

if __FILE__ == $0
  include DiffFile
  eg_dir   = File.join(File.dirname(__FILE__), %w(.. example))
  filename = {}
  %w(gcd gcd1 gcd-xx).each do |short|
    filename[short] = File.join(eg_dir, short + '.rb')
  end
  puts diff_file(filename['gcd'], filename['gcd1']).join("\n")
  puts diff_file(filename['gcd'], filename['gcd-xx']).join("\n")
end
