# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::ShowTraceBuffer < Trepan::ShowBoolSubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD} [NUM]

Show the events recorded in the event buffer. If NUM is a negative
number, events run starting from that many debugger stops back. If NUM
is a positive number, we print starting from that (adjusted) position
in the event buffer with Since the event buffer may be a ring, its
zero being the first position. (Since the event buffer may be a ring
the earliest position recorded may move around.)
    EOH
    MIN_ABBREV   = 'b'.size
    SHORT_HELP   = "Show tracing buffer"
  end

  def parse_show_buffer_args(args)
    marksize = @proc.eventbuf.size
    opts = {
      :max_value    => @proc.eventbuf.size,
      :min_value    => - marksize,
      :msg_on_error => 
      'Positive or negative number expected, got %s.' % args[0]
    }
    num = @proc.get_an_int(args[0], opts)
    return nil, nil unless num
    num = marksize + num  if num < 0 
    first = @proc.eventbuf.zero_pos + num
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
  require_relative '../trace'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::ShowTrace,
                                   Trepan::SubSubcommand::ShowTraceBuffer)
end
