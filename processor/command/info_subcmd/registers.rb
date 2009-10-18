# -*- coding: utf-8 -*-
require_relative %w(.. base subsubcmd)
require_relative %w(.. base subsubmgr)

class Debugger::SubSubcommand::InfoRegisters < Debugger::SubSubcommandMgr
  unless defined?(HELP)
    HELP         = 
'List of contents for the registers of the current stack frame.
A register name given as an argument lists only that register.

Examples:
  info registers     # show all registers
  info register pc   # show only the pc register
  info reg sp        # show stack pointer register: sp(0)
  info reg sp 1      # show sp(1)
'

    MIN_ABBREV   = 'reg'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info registers)
  end

  def run(args)

    args = @parent.last_args
    all_regs = @subcmds.subcmds.keys

    if args.size == 2
      # Form is: "info registers"
      all_regs.sort.each do |subcmd_name|
        @subcmds.subcmds[subcmd_name].run([])
      end
    else
      subcmd_name = args[2]
      key_name    = 'inforegisters' + subcmd_name
      remain_args = args[3..-1]
      if all_regs.member?(key_name)
        @subcmds.subcmds[key_name].run(remain_args) 
      else
        errmsg("info registers: %s is not a valid register name" % subcmd_name)
        nil
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
