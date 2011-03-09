# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoBreakpoints < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<EOH
info breakpoints [num1 ...] [verbose]

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
    SHORT_HELP = "Status of user-settable breakpoints"
  end

  def bpprint(bp, verbose=false)
    disp  = bp.temp?    ? 'del  ' : 'keep '
    disp += bp.enabled? ? 'y  '   : 'n  '

    iseq = bp.iseq
    mess = '%-4dbreakpoint    %s at ' % [bp.id, disp]

    line_loc = '%s:%d' % 
      [iseq.source_container.join(' '),
       iseq.offset2lines(bp.offset).join(', ')]
    vm_loc = "VM offset %d of instruction sequence \"%s\"" %
      [bp.offset, iseq.name]

    loc, other_loc =
      if 'line' == bp.type
        [line_loc, vm_loc]
      else # 'offset' == bp.type
        [vm_loc, line_loc]
      end
    msg(mess + loc)
    msg("\t#{other_loc}") if verbose

    if bp.condition && bp.condition != 'true'
      msg("\tstop %s %s" %
          [bp.negate ? "unless" : "only if", bp.condition])
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

  def save_command
    bpmgr = @proc.brkpts
    bpmgr.list.inject([]) do |res, bp|
      iseq = bp.iseq
      next unless 'file' == iseq.source_container[0]
      loc = iseq.source_container[1] + ':'
      loc += 
        # if 'line' == bp.type
          iseq.offset2lines(bp.offset)[0].to_s
        # else
        #  'O' + bp.offset.to_s
        # end
      res << "break #{loc}"
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
        "An '#{PREFIX.join(' ')}' argument must eval to a breakpoint between 1..#{@proc.brkpts.max}.",
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
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Trepan::Subcommand::InfoBreak.new(cmd)

  puts '-' * 20
  subcommand.run(%w(info break))
  puts '-' * 20
  subcommand.summary_help(name)
  puts
  puts '-' * 20

  require 'thread_frame'
  tf = RubyVM::ThreadFrame.current
  pc_offset = tf.pc_offset
  def foo
    5 
  end
  
  brk_cmd = dbgr.core.processor.commands['break']
  brk_cmd.run(['break', "O#{pc_offset}"])
  cmd.run(%w(info break))
  puts '-' * 20
  brk_cmd.run(['break', 'foo'])
  subcommand.run(%w(info break))
  puts '-' * 20
  p subcommand.save_command

end
