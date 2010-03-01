# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)
require_relative %w(.. .. .. app frame)

class Debugger::Subcommand::InfoArgs < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show argument variables of the current stack frame'
    MIN_ABBREV   = 'ar'.size 
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info args)
  end

  include Debugger::Frame
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
      param_names = all_param_names(@proc.frame.iseq, false)
      if param_names.empty?
        msg("No parameters in call.")
      else
        param_names.each_with_index do |var_name, i|
          var_value = @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
          msg("#{var_name} = #{var_value}")
        end
        unless 'call' == @proc.core.event and 0 == @proc.frame_index
          msg("Values may have change from the initial call values.")
        end
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
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoArgs.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
