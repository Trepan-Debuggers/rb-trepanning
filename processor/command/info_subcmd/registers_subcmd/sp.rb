# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative 'helper'

class Trepan::Subcommand::InfoRegistersSp < Trepan::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Show information about the VM stack pointer (SP).

usage: 
   info register sp [NUMBER NUMBER ...|size]

With no arguments, all SP values for the current frame of the debugged
program are shown.  If a number is given, then the entry at that
location is shown. If "size" is given, then we show the number items
in the stack of the current frame.

The VM uses a stack to store temporary values in computations. For
example to compute "a + b", the values of "a" and "b" are pushed onto
a stack pointed to by SP. Just before the addition is perofrmed, sp(1)
will have the value "a" contians and sp(2) will contain the value of
"b"

See also "info register LFP"'

    MIN_ABBREV   = 'sp'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info registers sp)
  end

  include Registers
  def run(args)
    if args.size == 0
      1.upto(@proc.frame.sp_size-1) do |i|
        msg "%s%d: %s" % [' ' * 2, i, @proc.frame.sp(i).inspect]
      end if @proc.frame.sp_size
    elsif args.size == 1 and 'size' == args[0] 
      msg "Number of stack items in frame is #{@proc.frame.sp_size}."
    else
      args.each do |arg|
        register_array_index(PREFIX[-1], arg, @proc.frame.sp_size)
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, info_cmd = MockDebugger::setup('info')
  testcmdMgr = Trepan::Subcmd.new(info_cmd)
  cmd_name   = Trepan::SubSubcommand::InfoRegistersSp::PREFIX.join('')
  infox_cmd  = Trepan::SubSubcommand::InfoRegistersSp.new(info_cmd.proc, 
                                                          info_cmd,
                                                          cmd_name)
  infox_cmd.summary_help(name)
  puts
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  infox_cmd.run([])
  puts
  %w(0 1 10).each do |val|
    infox_cmd.run([val])
    puts '-' * 40
  end
  infox_cmd.run(['size'])

  name = File.basename(__FILE__, '.rb')
end
