# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require 'thread_frame'
require_relative 'virtual'
class Trepan

    class CmdProcessor < VirtualCmdProcessor

        attr_reader   :brkpts          # BreakpointManager.

        attr_reader   :brkpt           # Breakpoint. If we are stopped at a
                                       # breakpoint this is the one we
                                       # found.  (There may be other
                                       # breakpoints that would have caused a stop
                                       # as well; this is just one of them).
                                       # If no breakpoint stop this is nil.

        def breakpoint_initialize
            @brkpts = BreakpointMgr.new
            @brkpt  = nil
        end

        def breakpoint_finalize
            @brkpts.finalize
        end

        def breakpoint?
            @brkpt = @brkpts.find(@frame.iseq, @frame.pc_offset, @frame.binding)
            @brkpts.delete_by_brkpt(@brkpt) if @brkpt && @brkpt.temp?
            return !!@brkpt
        end

        def breakpoint_find(bpnum, show_errmsg = true)
            if 0 == @brkpts.size
                errmsg('No breakpoints set.') if show_errmsg
                return nil
            elsif bpnum > @brkpts.max || bpnum < 1
                errmsg('Breakpoint number %d is out of range 1..%d' %
                       [bpnum, @brkpts.max]) if show_errmsg
                return nil
            end
            bp = @brkpts[bpnum]
            if bp
                return bp
            else
                errmsg('Breakpoint number %d previously deleted.' %
                       bpnum) if show_errmsg
                return nil
            end
        end

        # Does whatever needs to be done to set a breakpoint
        def breakpoint_line(line_number, initial_iseq, temp=false)
            # FIXME: handle breakpoint conditions.
            found_iseq = initial_iseq.child_iseqs.detect do |iseq|
                iseq.lineoffsets.keys.member?(line_number)
            end
            unless found_iseq
                found_iseq = initial_iseq.parent
                while found_iseq do
                    break if found_iseq.lineoffsets.keys.member?(line_number)
                    found_iseq = found_iseq.parent
                end
            end
            offset =
                if found_iseq
                    # FIXME
                    found_iseq.line2offsets(line_number)[1] ||
                        found_iseq.line2offsets(line_number)[0]
                else
                    nil
                end
            unless offset
                place = "in #{iseq.source_container.join(' ')} " if found_iseq
                errmsg("No line #{line_number} found #{place}for breakpoint.")
                return nil
            end
            @brkpts.add(found_iseq, offset, :temp => temp)
        end

        def breakpoint_offset(offset, iseq, condition, negate, temp=false)
            # FIXME: handle breakpoint conditions.
            unless iseq.offsetlines.keys.member?(offset)
                errmsg("Offset #{offset} not found in #{iseq.label} for breakpoint.")
                return nil
            end
            @brkpts.add(iseq, offset, :temp => temp, :condition => condition,
                        :negate => negate, :type => 'offset')
        end

        # Delete a breakpoint given its breakpoint number.
        def delete_breakpoint_by_number(bpnum, do_enable=true)
            bp = breakpoint_find(bpnum)
            return false unless bp

            @brkpts.delete_by_brkpt(bp)
            return true
        end

        # Enable or disable a breakpoint given its breakpoint number.
        def en_disable_breakpoint_by_number(bpnum, do_enable=true)
            bp = breakpoint_find(bpnum)
            return false unless bp

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
