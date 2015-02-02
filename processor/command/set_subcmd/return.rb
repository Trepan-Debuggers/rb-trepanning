# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Trepan::Subcommand::SetReturn < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
**#{PREFIX.join(' ')}** *expression*

Set return value to *expression*

There are currently a number of restrictions here. First, you have to
be at some sort of return event. Second, the opcode you are stopped at
has to be a `leave` instruction.

Warning: this is potentially dangerous.

See also:
---------

`set register sp`, `info register sp`
EOH
    IN_LIST      = true
    MIN_ABBREV   = 'ret'.size
  end

  include Trepan::Frame

  def run(args)
      event = @proc.event.to_s
      unless %w(return c_return b_return).member?(event)
          errmsg('You need to be in a return event to do this. Event is %s' %
                 event)
          return
      end
      unless @proc.core.trace_point
          msg('We need a trace-point to get this information')
          return
      end
      if args.size < 3
          errmsg "Too few arguments - the 'return' command requires a return value"
          return
      end
      if %w(return b_return).member?(event)
          index = 1
      else
          index = 3
      end
      frame = @proc.frame
      iseq = frame.iseq
      unless iseq
          msg "Can't set return for C functions yet"
          return
      end
      opname = iseq.op_at(frame.pc_offset)
      @proc.commands['set'].run(["set", "sp", index.to_s, *args[2..-1]])
  end
end

if __FILE__ == $0
    # Demo it.
    require_relative '../../mock'
    require_relative '../../subcmd'
    name = File.basename(__FILE__, '.rb')

    # FIXME: DRY the below code
    dbgr, cmd = MockTrepan::setup('set')
    subcommand = Trepan::Subcommand::SetReturn.new(cmd)
    testcmdMgr = Trepan::Subcmd.new(subcommand)

    def subcommand.msg(message)
        puts message
    end
    def subcommand.msg_nocr(message)
        print message
    end
    def subcommand.errmsg(message)
        puts message
    end
    subcommand.run(%w(20))
    name = File.basename(__FILE__, '.rb')
    subcommand.summary_help(name)
end
