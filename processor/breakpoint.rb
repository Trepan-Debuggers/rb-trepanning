require_relative %w(.. lib core)
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
        errmsg("Line number #{line_number} not found for breakpoint")
        return nil
      end
      @brkpts.add(temp, offset, iseq)
    end

    def breakpoint_offset(offset, iseq, temp=false)
      # FIXME: handle breakpoint conditions.
      unless iseq.offsetlines.keys.member?(offset)
        errmsg("Offset #{offset} not found in #{iseq.name} for breakpoint")
        return nil
      end
      @brkpts.add(temp, offset, iseq)
    end

    # Enable or disable a breakpoint given its breakpoint number.
    def en_disable_breakpoint_by_number(bpnum, do_enable=true)
      bp = @brkpts[bpnum]
      unless bp
        msg('Breakpoint %d not found.' % bpnum)
        return false
      end
          
      en = do_enable ? 'en' : 'dis'
      if bp.enabled? == do_enable
        msg('Breakpoint (%d) previously %sabled' % 
            [bpnum, endis])
        return false
      end
      bp.enabled = do_enable
    end
  end
end
