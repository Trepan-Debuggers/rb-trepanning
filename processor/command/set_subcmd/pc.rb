# -*- coding: utf-8 -*-
# Copyright (C) 2012, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetPC < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
**#{PREFIX.join(' ')}** *integer-expression*

Set VM program-counter register (PC) to *integer-expression*

Warning: this is potentially dangerous.

See also:
---------

`set register sp`, `info register sp`
EOH
    SHORT_HELP   = 'Set VM program counter (PC)'
    IN_LIST      = true
    MIN_ABBREV   = 'pc'.size
  end

  def run(args)
    if args.size < 3
      errmsg "Too few arguments - the 'pc' command requires a value"
      return
    end
    new_val_str = args[2..-1].join(' ')
    begin
      new_val = @proc.debug_eval(new_val_str)
    rescue StandardError, ScriptError => e
      return
    end
    msg("Old value was: %s" % @proc.frame.pc_offset.inspect)
    @proc.frame.pc_offset =  new_val
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
  subcommand = Debugger::Subcommand::SetPC.new(cmd)
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
