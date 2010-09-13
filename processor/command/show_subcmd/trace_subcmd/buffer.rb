# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::ShowTraceBuffer < Debugger::ShowBoolSubSubcommand
  unless defined?(HELP)
    HELP         = 
"show trace buffer [NUM]

Show the events recorded in the event buffer. If NUM is a negative
number, events run starting from that many debugger stops back. If NUM
is a positive number, we print starting from that (adjusted) position
in the event buffer with Since the event buffer may be a ring, its
zero being the first position. (Since the event buffer may be a ring
the earliest position recorded may move around.)
"
    MIN_ABBREV   = 'b'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show trace buffer)
    SHORT_HELP   = "Show tracing buffer"
  end

  def parse_show_buffer_args(args)
    marksize = @proc.eventbuf.marks.size
    opts = {
      :max_value    => @proc.eventbuf.size,
      :min_value    => - marksize,
      :msg_on_error => 
      'Positive or negative number expected, got %s.' % args[0]
    }
    num = @proc.get_an_int(args[0], opts)
    return nil, nil unless num
    first = 
      if num < 0 
        @proc.eventbuf.marks[marksize+num] 
      else
        @proc.eventbuf.zero_pos + num
      end
    return first, nil
  end

  def run(args)
    if args.size > 1
      if @proc.eventbuf.size ==  0 
        msg 'no events recorded.'
        errmsg 'event buffer tracing is off. ' + 
          'use "set trace buffer on" to turn on.' unless 
          settings[:tracebuffer]
        return
      end
      first, last = parse_show_buffer_args(args[1..-1])

      return unless first
      @proc.eventbuf_print(first, last, settings[:maxwidth])
    else
      super
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, show_cmd = MockDebugger::setup('show')
  testcmdMgr     = Debugger::Subcmd.new(show_cmd)
  trace_cmd      = Debugger::SubSubcommand::ShowTrace.new(dbgr.core.processor, 
                                                          show_cmd)

  # FIXME: remove the 'join' below
  cmd_name       = Debugger::SubSubcommand::ShowTraceBuffer::PREFIX.join('')
  tb_cmd         = Debugger::SubSubcommand::ShowTraceBuffer.new(show_cmd.proc, 
                                                                trace_cmd,
                                                                cmd_name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  tb_cmd.run([])

end
