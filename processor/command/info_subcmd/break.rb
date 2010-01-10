# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::InfoBreak < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show Breakpoints'
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
    if bp.condition != 'true'
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
      verbose = 'verbose' == args[0]
    end

    bpmgr = @proc.brkpts
    if bpmgr.empty?  
      msg('No breakpoints.')
    else
      # There's at least one
      msg("Num Type          Disp Enb Where")
      bpmgr.list.each do |bp|
          bpprint(bp, verbose)
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
