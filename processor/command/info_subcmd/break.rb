# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::InfoBreak < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = <<EOH
info break [num1 ...] [verbose]

Show status of user-settable breakpoints. If no breakpoint numbers are
given, the show all breakpoints. Otherwise only those breakpoints
listed are shown and the order given. If VERBOSE is given, more
information provided about each breakpoint.

The "Disp" column contains one of "keep", "del", the disposition of
the breakpoint after it gets hit.

The "enb" column indicates whether the breakpoint is enabled.

The "Where" column indicates where the breakpoint is located.
EOH

    MIN_ABBREV   = 'br'.size 
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(info break)
    SHORT_HELP = "Status of user-settable breakpoints"
  end

  def bpprint(bp, verbose=false)
    disp  = bp.temp?    ? 'del  ' : 'keep '
    disp += bp.enabled? ? 'y  '   : 'n  '

    iseq = bp.iseq
    mess = '%-4dbreakpoint    %s at %s:%d' %
      [bp.id, disp, iseq.source_container.join(' '),
         iseq.offset2lines(bp.offset).join(', ')]

    msg(mess)
    if verbose
      msg("\tVM offset %d of instruction sequence %s" % 
          [bp.offset, iseq.name])
    end
    if bp.condition && bp.condition != 'true'
      msg("\tstop only if %s" % bp.condition)
    end
    if bp.ignore > 0
      msg("\tignore next %d hits" % bp.ignore)
    end
    if bp.hits > 0
      ss = (bp.hits > 1) ? 's' : ''
      msg("\tbreakpoint already hit %d time%s" %
          [bp.hits, ss])
    end
  end

  def run(args)
    verbose = false
    unless args.empty?
      if 'verbose' == args[-1]
        verbose = true
        args.pop
      end
    end


    show_all = 
      if args.size > 2
        opts = {
        :msg_on_error => 
        "An 'info break'  argument must eval to a breakpoint between 1..#{@proc.brkpts.max}.",
        :min_value => 1,
        :max_value => @proc.brkpts.max
      }
        bp_nums = @proc.get_int_list(args[2..-1])
        false
      else
        true
      end
    
    bpmgr = @proc.brkpts
    if bpmgr.empty?  
      msg('No breakpoints.')
    else
      # There's at least one
      msg("Num Type          Disp Enb Where")
      if show_all
        bpmgr.list.each do |bp|
          bpprint(bp, verbose)
        end
      else
        notfound = []
        bp_nums.each do |bp_num|
          bp = @proc.brkpts[bp_num]
          if bp
            bpprint(bp, verbose)
          else
            notfound << bp_num
          end
        end
        errmsg "No breakpoint number(s) #{not_found.join(' ')}." unless
          notfound.empty? 
      end
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoBreak.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
