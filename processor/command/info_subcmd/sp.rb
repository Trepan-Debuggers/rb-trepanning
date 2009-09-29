# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::InfoSp < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM stack pointer'
    MIN_ABBREV   = 'fr'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
  end

  def run(args)
    if args.size == 0
      # Form is: "info sp" which means "info sp 1"
      position = 1
    else
      position_str = args[0]
      opts = {
        :msg_on_error => 
        "The 'info sp' command argument must eval to an integer. Got: %s" % position_str,
        # :min_value => 1,
        # :max_value => ??
      }
      position = @proc.get_an_int(position_str, opts)
      return unless position
    end
    msg("VM stack %d: %s" % [position, @proc.frame.sp(position)])
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::InfoSp.new(cmd)
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
  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
