# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Trepan::Subcommand::InfoArgs < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Show argument variables of the current stack frame'
    MIN_ABBREV   = 'ar'.size 
    MIN_ARGS     = 0
    MAX_ARGS     = 0
    NEED_STACK   = true
  end

  def run(args)
    if 'CFUNC' == @proc.frame.type
      argc = @proc.frame.argc
      if argc > 0 
        1.upto(argc).each do |i| 
          msg "#{i}: #{@proc.frame.sp(argc-i+3).inspect}"
        end
      else
        msg("No parameters in C call.")
      end
    else
      param_names = Trepan::Frame::all_param_names(@proc.frame.iseq, false)
      if param_names.empty?
        msg("No parameters in call.")
      else
        param_names.each_with_index do |var_name, i|
          var_value = 
            @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
          msg("#{var_name} = #{var_value}")
        end
        unless 'call' == @proc.event and 0 == @proc.frame_index
          msg("Values may have change from the initial call values.")
        end
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoArgs, false)
  cmd.run(cmd.prefix)
end
