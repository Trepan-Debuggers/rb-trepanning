# -*- coding: utf-8 -*-
require 'columnize'
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Debugger::Subcommand::InfoLocals < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show local variables of the current stack frame'
    MAX_ARGS     = 1
    MIN_ABBREV   = 'lo'.size 
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info locals)
  end

  include Debugger::Frame
  def get_local_names
    iseq = @proc.frame.iseq
    0.upto(iseq.local_size-2).map do
      |i|
      iseq.local_name(i)
    end
  end

  def run(args)
    if args.size == 3
      if 0 == 'names'.index(args[-1].downcase)
        if 'CFUNC' == @proc.frame.type
          errmsg('info local names not supported for C frames')
        else
          local_names = get_local_names()
          if local_names.empty?
            msg "No local variables defined."
          else
            msg "Local variable names:"
            width = settings[:maximumwidth]
            mess = Columnize::columnize(local_names, 
                                        @proc.settings[:maximumwidth], ', ',
                                        false, true, ' ' * 2).chomp
            msg mess
          end
        end
      else
        errmsg("unrecognized argument #{args[2]}")
      end
    elsif args.size == 2
      if 'CFUNC' == @proc.frame.type
        argc = @proc.frame.argc
        if argc > 0 
          1.upto(argc).each do |i| 
            msg "#{i}: #{@proc.frame.sp(argc-i+3).inspect}"
          end
        else
          msg("No parameters in C call; showing other C locals is not supported.")
        end
      else
        local_names = get_local_names
        if local_names.empty?
          msg "No local variables defined."
        else
          msg "Local variables:"
          get_local_names.each_with_index do |var_name, i| 
            var_value = @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
            msg("#{var_name} = #{var_value}")
          end
        end
      end
    else
      errmsg("Wrong number of arguments #{args.size}")
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoLocals.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
