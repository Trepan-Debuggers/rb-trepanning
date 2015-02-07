# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoReturn < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
**#{CMD}**

Show the value about to be returned.

You have to be at some sort of return event for this command to work.

See also:
---------

`set return`, `info frame`, `info program`

EOH
    MIN_ABBREV   = 'ret'.size # Note we have "info registers"
    NEED_STACK   = true
  end

  def run(args)
      event = @proc.event
      frame = @proc.frame
      if %w(return b_return).member?(event.to_s)
          ret_val = frame.sp(1)
          msg('Return class: %s' % ret_val.class)
          msg('Return value: %s' % ret_val.inspect)
      elsif %w(c_return).member?(event.to_s)
          ret_val = frame.sp(frame.argc + 3)
          msg('Return class: %s' % ret_val.class)
          msg('Return value: %s' % ret_val.inspect)
      else
          errmsg('You need to be in a return event to do this. Event is %s' %
                 event)
      end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Trepan::Subcommand::InfoReturn.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
