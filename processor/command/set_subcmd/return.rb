# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Trepan::Subcommand::SetReturn < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Set the value that will be returned in the current method'
    IN_LIST      = true
    MIN_ABBREV   = 'ret'.size
  end

  include Trepan::Frame

  def run(args)
      event = @proc.event
      unless %w(return c_return b_return).member?(event.to_s)
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
      new_val_str = args[2..-1].join(' ')
      begin
          new_val = @proc.debug_eval(new_val_str)
      rescue StandardError, ScriptError => e
          p $!
          return
      end
      ret_val = @proc.core.trace_point.return_value
      msg('Return value was: %s' % ret_val.inspect)
      @proc.core.trace_point.return_value = new_val
      msg('New value is: %s' % new_val.inspect)
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
