module Trepanning
  # Returns a RubyVM::Instruction for the specified line. We search the
  # current instruction sequence +iseq+ and then up the parent scope.  If we hit
  # the top and we can't find +line+ that way, then we
  # reverse the search from the top and search down. This will add
  # all siblings of ancestors of +meth+.
  # Similar to rbx-trepanning method "find_method_with_line".
  def find_iseq_with_line_from_iseq(iseq, lineno, go_up=true)
    find_iseq_with_line_from_iseq2(iseq, lineno, go_up, {})
  end

  def find_iseq_with_line_from_iseq2(iseq, lineno, go_up, seen)
    return iseq if iseq.offsetlines.values.flatten.uniq.member?(lineno)
    seen[iseq] = true
    prev_iseq = iseq
    while prev_iseq = prev_iseq.parent
      iseq = prev_iseq
      return iseq if iseq.offsetlines.values.flatten.uniq.member?(lineno)
    end if go_up
    # At top and not found so now go down..
    iseq.child_iseqs.each do |child_iseq|
      next if seen[child_iseq] # we tried before
      # puts "#{child_iseq.name}, #{child_iseq}"
      result = find_iseq_with_line_from_iseq2(child_iseq, lineno, false, seen)
      return result if result
    end
    return nil
  end
end
