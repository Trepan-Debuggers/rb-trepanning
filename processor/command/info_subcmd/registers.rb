# -*- coding: utf-8 -*-
require_relative %w(.. base_subcmd)

class Debugger::Subcommand::InfoRegisters < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 
'List of registers and their contents, for selected stack frame.
Register name as argument means describe only that register.'

    MIN_ABBREV   = 'r'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    SHORT_STACK  = 'List of registers and their contents'
    ALL_ARGS     = %w(pc sp lfp)  # Put in order you want listed
  end

  def run(args)

    if args.size == 0
      # Form is: "info registers"
      list = ALL_ARGS
    else
      list = args.map do |arg|
        if ALL_ARGS.member?(arg)
          arg
        else
          errmsg("info registers: %s is not a valid register name" % arg)
          nil
        end
      end
    end

    list.each do |arg|
      case arg
        when 'lfp'
        msg("lfp(0): %s" % @proc.frame.lfp(0))
        when 'pc' 
        msg "pc: %d" % @proc.frame.pc_offset
        when 'sp'
        msg("sp(0): %s" % @proc.frame.sp(0))
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('exit')
  subcommand = Debugger::Subcommand::InfoRegisters.new(cmd)
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
