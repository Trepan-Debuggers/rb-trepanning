# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative 'helper'

class Trepan::Subcommand::InfoRegistersSp < Trepan::SubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP = <<EOH
**#{CMD}** [*number* *number*...| **size**]

With no arguments, all *sp* values for the current frame of the debugged
program are shown.  If a number is given, then the entry at that
location is shown. If `size` is given, then we show the number items
in the stack of the current frame.

The VM uses a stack to store both local variables and temporary values
in computations. The stack grows downward (toward lower numbers).
As a result, local values and parameters in the stack in the reverse
order as they appear in a call and are as they are defined.

The entry before the end of the stack contains the return value and
the top entry is a fake RubyVM::Env value.

See also:
---------

`info register ep`, `info register pc`
EOH

    MIN_ABBREV   = 'sp'.size
    NEED_STACK   = true
    SHORT_HELP   = "Show value(s) of the VM stack pointer (SP)."
  end

    include Registers
    def run(args)
        if args.size == 0
            1.upto(@proc.frame.sp_size) do |i|
                val = @proc.frame.sp(i).inspect
                klass = @proc.frame.sp(i).class
                msg "%s%d: %s (%s)" % [' ' * 2, i, val, klass]
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
  require_relative '../registers'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::InfoRegisters,
                                   Trepan::SubSubcommand::InfoRegistersSp,
                                   false)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new
  # dbgr.debugger
  cmd.run([])
  puts
  %w(0 1 10).each do |val|
    cmd.run([val])
    puts '-' * 40
  end
  cmd.run(['size'])
end
