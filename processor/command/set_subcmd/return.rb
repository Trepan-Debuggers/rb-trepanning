# -*- coding: utf-8 -*-
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Debugger::Subcommand::SetReturn < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Set the value that will be returned in the current method'
    IN_LIST      = true
    MIN_ABBREV   = 'ret'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set return)
  end

  include Debugger::Frame

  def run(args)
    event = @proc.core.event
    unless %w(return c-return).member?(event)
      errmsg('You need to be in a return event to do this. Event is %s' % 
             event)
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
    ret_val = Debugger::Frame.value_returned(@proc.frame, event)
    msg('Return value was: %s' % ret_val.inspect)
    Debugger::Frame.set_return_value(@proc.frame, event, new_val)
    msg('New value is: %s' % new_val.inspect)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetReturn.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

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
