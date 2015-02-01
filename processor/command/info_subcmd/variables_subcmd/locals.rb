# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require 'columnize'
require_relative '../../base/subsubcmd'
require_relative '../../../../app/frame'
require_relative '../../../../app/util'

class Trepan::Subcommand::InfoVariablesLocals < Trepan::SubSubcommand
  Trepan::Util.suppress_warnings {
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
**#{CMD}** [ **--names** | *local-number* ]

Show local variables including parameters of the current stack frame.
Normally for each which show both the name and value. If you just
want a list of names add parameter `--names`.

If you just want the name and value for a particular number give that number.

Examples:
---------

    info variables local
    info variables local 0
    info variables local --names

See Also:
---------

`info variables`, `info globals`, `info variables constants`,
and `info variables class`

EOH
    SHORT_HELP   = 'Show local variables of the current stack frame'
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    MIN_ABBREV   = 'lo'.size
    NEED_STACK   = true
  }

    def complete(prefix)
        if @proc && @proc.frame
            argc =
                if 'CFUNC' == @proc.frame.type
                    @proc.frame.argc
                else
                    iseq = @proc.frame.iseq
                    iseq.local_size - 2
                end
            ary = (0..argc).map{|i| i.to_s}
            ['--names'] + ary
        else
            []
        end
    end

    def names
        iseq = @proc.frame.iseq
        0.upto(iseq.local_size-2).map { |i| iseq.local_name(i) }
    end

    def run_for_locals(args, klass=nil)
        suffix = klass ? " for #{klass.to_s}" : '' rescue ''
        c_frame = 'CFUNC' == @proc.frame.type
        if args.size == 2
            last_arg = args[-1]
            argc =
                if c_frame
                    @proc.frame.argc
                else
                    iseq = @proc.frame.iseq
                    iseq.local_size - 2
                end
            if 0 == '--names'.index(last_arg)
                if c_frame
                    msg("Can't show parameter names in a C call")
                    return
                end
                if names.empty?
                    msg "No local variables defined."
                else
                    section "Local variable names#{suffix}:"
                    width = settings[:maxwidth]
                    mess = Columnize::columnize(names,
                                                @proc.settings[:maxwidth], '  ',
                                                false, true, ' ' * 2).chomp
                    msg mess
                end
            else
                if c_frame
                    val = @proc.get_an_int(last_arg,
                                           :max_value => argc,
                                           :min_value => 0,
                                           )
                    msg "#{val}: #{@proc.frame.sp(argc-val+3).inspect}"
                elsif names.member?(last_arg)
                    var_value =
                        @proc.safe_rep(@proc.debug_eval_no_errmsg(last_arg).inspect)
                    msg("#{last_arg} = #{var_value}", :code => true)
                else
                    val = @proc.get_an_int(last_arg,
                                           :max_value => argc,
                                           :min_value => 0,
                                           )
                    return unless val
                    var_name = @proc.frame.iseq.local_name(val)
                    var_class =
                        @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).class)
                    var_value =
                        @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
                    msg("#{var_name} = #{var_value} (#{var_class})",
                        :code => true)
                end
            end
        elsif args.size == 1
            if c_frame
                argc = @proc.frame.argc
                if argc > 0
                    1.upto(argc).each do |i|
                        msg "#{i}: #{@proc.frame.sp(argc-i+3).inspect}"
                    end
                else
                    msg("No parameters in C call; showing other C locals is not supported.")
                end
            else
                if names.empty?
                    msg "No local variables defined#{suffix}."
                else
                    section "Local variables#{suffix}:"
                    names.each do |var_name|
                        var_value =
                            @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
                        msg("#{var_name} = #{var_value}", :code => true)
                    end
                end
            end
        else
            errmsg("Wrong number of arguments #{args.size}")
        end
    end

    def run(args)
        run_for_locals(args, @proc.debug_eval('self'))
    end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../variables'
  # cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::InfoVariables,
  #                                  Trepan::SubSubcommand::InfoVariablesLocals
  #                                  )
  # cmd.run([])
  # cmd.run(['name'])
end
