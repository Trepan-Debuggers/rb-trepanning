require_relative %w(.. lib core)
class Debugger
  class CmdProcessor
    # Does whatever needs to be done to set a breakpoint
    def breakpoint(number, is_offset = false)
      # FIXME: turn line_number into a condition.
      if number
        if is_offset 
          offset = number 
          iseq = @frame.iseq
        else 
          iseq = @frame.iseq.child_iseqs.detect do |iseq|
            iseq.lineoffsets.keys.member?(number) end
          offset = iseq ? iseq.line2offsets(number)[1] : nil
        end
        unless offset
          errmsg("Line number #{number} not found for breakpoint")
          return nil
        end
        return @brkpts.add(false, offset, iseq)
      end
      return nil
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
