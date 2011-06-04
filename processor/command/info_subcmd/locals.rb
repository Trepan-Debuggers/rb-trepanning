# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'columnize'
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Trepan::Subcommand::InfoLocals < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Show local variables of the current stack frame'
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    MIN_ABBREV   = 'lo'.size 
    NEED_STACK   = true
  end

  include Trepan::Frame
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
            section "Local variable names:"
            width = settings[:maxwidth]
            mess = Columnize::columnize(local_names, 
                                        @proc.settings[:maxwidth], ', ',
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
          section "Local variables:"
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
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoLocals, false)
  cmd.run(cmd.prefix)
  cmd.run(cmd.prefix + ['name'])
end
