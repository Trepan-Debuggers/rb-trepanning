# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::SetReturn < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Set the value that will be returned in the current method'
    IN_LIST      = true
    MIN_ABBREV   = 'ret'.size
    NAME         = File.basename(__FILE__, '.rb')
  end

  def run(args)
    unless %w(return).member?(@proc.core.event)
      errmsg("You need to be in a return event to do this. Event is %s" % 
             @proc.core.event)
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
    msg("Return value was: %s" % @proc.frame.sp(1).inspect)
    @proc.frame.sp_set(1, new_val).inspect
    msg("New value is: %s" % new_val.inspect)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
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
