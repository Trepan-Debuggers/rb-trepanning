# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'columnize'
require_relative '../base/subcmd'
require_relative '../../../app/frame'
require_relative '../../../app/util'

class Trepan::Subcommand::InfoLocals < Trepan::Subcommand
  Trepan::Util.suppress_warnings {
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD}
#{CMD} [names]

Show local variables including parameters of the current stack frame.
Normally for each which show both the name and value. If you just
want a list of names add parameter 'names'.
EOH
    SHORT_HELP   = 'Show local variables of the current stack frame'
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    MIN_ABBREV   = 'lo'.size 
    NEED_STACK   = true
  }

  def get_names
    iseq = @proc.frame.iseq
    0.upto(iseq.local_size-2).map do
      |i|
      iseq.local_name(i)
    end
  end

  def run_for_type(args, type)
    if args.size == 3
      if 0 == 'names'.index(args[-1].downcase)
        if 'CFUNC' == @proc.frame.type
          errmsg("info #{type} names not supported for C frames")
        else
          names = get_names()
          if names.empty?
            msg "No #{type} variables defined."
          else
            section "#{type.capitalize} variable names:"
            width = settings[:maxwidth]
            mess = Columnize::columnize(names, 
                                        @proc.settings[:maxwidth], '  ',
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
        names = get_names
        if names.empty?
          msg "No #{type} variables defined."
        else
          section "#{type.capitalize} variables:"
          get_names.each_with_index do |var_name, i| 
            var_value = @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
            msg("#{var_name} = #{var_value}")
          end
        end
      end
    else
      errmsg("Wrong number of arguments #{args.size}")
    end
  end
  def run(args)
    run_for_type(args, 'local')
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoLocals, false)
  cmd.run(cmd.prefix)
  cmd.run(cmd.prefix + ['name'])
end
