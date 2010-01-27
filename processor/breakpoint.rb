require_relative %w(.. app core)
class Debugger
  class CmdProcessor
    # Does whatever needs to be done to set a breakpoint
    def breakpoint_line(line_number, iseq, temp=false)
      # FIXME: handle breakpoint conditions.
      iseq = iseq.child_iseqs.detect do |iseq|
        iseq.lineoffsets.keys.member?(line_number) 
      end
      offset = iseq ? iseq.line2offsets(line_number)[1] : nil
      unless offset
        errmsg("Line number #{line_number} not found for breakpoint.")
        return nil
      end
      @brkpts.add(temp, offset, iseq)
    end

    def breakpoint_offset(offset, iseq, temp=false)
      # FIXME: handle breakpoint conditions.
      unless iseq.offsetlines.keys.member?(offset)
        errmsg("Offset #{offset} not found in #{iseq.name} for breakpoint.")
        return nil
      end
      @brkpts.add(temp, offset, iseq)
    end

    # Delete a breakpoint given its breakpoint number.
    def delete_breakpoint_by_number(bpnum, do_enable=true)
      bp = @brkpts[bpnum]
      unless bp
        errmsg('Breakpoint %d not found.' % bpnum)
        return false
      end
          
      @brkpts.delete_by_brkpt(bp)
      return true
    end

    # Enable or disable a breakpoint given its breakpoint number.
    def en_disable_breakpoint_by_number(bpnum, do_enable=true)
      bp = @brkpts[bpnum]
      unless bp
        errmsg('Breakpoint %d not found.' % bpnum)
        return false
      end
          
      enable_disable = do_enable ? 'en' : 'dis'
      if bp.enabled? == do_enable
        errmsg('Breakpoint %d previously %sabled.' % 
               [bpnum, enable_disable])
        return false
      end
      bp.enabled = do_enable
      return true
    end
  end
end
