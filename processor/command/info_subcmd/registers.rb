# -*- coding: utf-8 -*-
require_relative %w(.. base_subsubcmd)
require_relative %w(.. base_subsubmgr)

class Debugger::SubSubcommand::InfoRegisters < Debugger::SubSubcommandMgr
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

    args = @parent.last_args
    # require_relative %w(.. .. .. rbdbgr)
    # dbgr = Debugger.new(:set_restart => true)
    # dbgr.debugger(:immediate => true)
    if args.size == 2
      # Form is: "info registers"
      list = ALL_ARGS
    else
      list = args[2..-1].map do |arg|
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
        msg("lfp(0): %s" % @parent.proc.frame.lfp(0))
        when 'pc' 
        msg "pc: %d" % @parent.proc.frame.pc_offset
        when 'sp'
        msg("sp(0): %s" % @parent.proc.frame.sp(0))
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  info_cmd = cmds['info']
  command = Debugger::SubSubcommand::InfoRegisters.new(dbgr.core.processor,
                                                       info_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['info', name]
  info_cmd.instance_variable_set('@last_args', cmd_args)
  command.proc.frame_setup(RubyVM::ThreadFrame::current)
  command.run(cmd_args)
end
