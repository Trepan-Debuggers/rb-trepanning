# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)
require_relative %w(.. registers)

class Debugger::Subcommand::InfoRegistersSp < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Show the value of the VM stack pointer'
    MIN_ABBREV   = 'sp'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
  end

  def run(args)
    if args.size == 0
      # Form is: "info sp" which means "info sp 0"
      position = 0
    else
      position_str = args[0]
      opts = {
        :msg_on_error => 
        "The 'info registers sp' command argument must eval to an integer. Got: %s" % position_str,
        # :min_value => 1,
        # :max_value => ??
      }
      position = @proc.get_an_int(position_str, opts)
      return unless position
    end
    msg("VM sp(%d) = %s" % [position, @proc.frame.sp(position).inspect])
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, info_cmd = MockDebugger::setup('exit')
  testcmdMgr = Debugger::Subcmd.new(info_cmd)
  infox_cmd  = Debugger::SubSubcommand::InfoRegistersSp.new(info_cmd.proc,
                                                            info_cmd,
                                                            'inforegisters')
  # require_relative %w(.. .. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  infox_cmd.run([])

  def info_cmd.msg(message)
    puts message
  end
  def info_cmd.msg_nocr(message)
    print message
  end
  def info_cmd.errmsg(message)
    puts message
  end
  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
