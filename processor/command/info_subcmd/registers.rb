# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::InfoRegisters < Trepan::SubSubcommandMgr
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
#{CMD} [lfp|pc|sp]

List of contents for the registers of the current stack frame.
If a register name given, only only that register is show.

Examples:
#{CMD}     # show all registers
#{CMD} pc   # show only the pc register
info reg sp        # show all stack pointer registers
info reg sp 1 3    # show sp(1) and sp(3)
info reg sp size   # show sp size
info reg lfp       # show lfp(0)
    EOH
    MIN_ABBREV   = 'reg'.size  # Note we have "info return"
    NEED_STACK   = true
    SHORT_HELP   = 'List of register values of the current stack frame'
  end

  def run(args)

    args = @parent.last_args
    unavailable_regs = 
      if 'CFUNC' == @proc.frame.type
        %w(inforegisterslfp inforegisterspc) 
      else
        []
      end
    all_regs = @subcmds.subcmds.keys.sort - unavailable_regs
      
    if args.size == 2
      # Form is: "info registers"
      all_regs.sort.each do |subcmd_name|
        @subcmds.subcmds[subcmd_name].run([])
      end
    else
      subcmd_name = args[2]
      key_name    = PREFIX.join('') + subcmd_name
      remain_args = args[3..-1]
      if all_regs.member?(key_name)
        @subcmds.subcmds[key_name].run(remain_args) 
      elsif unavailable_regs.member?(key_name)
        msg("info registers: %s can not be displayed for frame type %s." % 
            [subcmd_name, @proc.frame.type])
      else
        errmsg("info registers: %s is not a valid register name" % subcmd_name)
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  info_cmd = cmds['info']
  command = Trepan::SubSubcommand::InfoRegisters.new(dbgr.core.processor,
                                                     info_cmd)
  name = File.basename(__FILE__, '.rb')
  cmd_args = ['info', name]
  info_cmd.instance_variable_set('@last_args', cmd_args)
  command.proc.frame_setup(RubyVM::ThreadFrame::current)
  command.run(cmd_args)
end
