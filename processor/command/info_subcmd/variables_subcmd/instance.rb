# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>

require_relative '../../base/subsubcmd'
require_relative '../../../../app/util'

class Trepan::Subcommand::InfoVariablesInstance < Trepan::SubSubcommand
    Trepan::Util.suppress_warnings {
        Trepanning::Subcommand.set_name_prefix(__FILE__, self)
        HELP         = <<-EOH
#{CMD}
#{CMD} [--names]

Show instance variables of the current stack frame.
Normally for each which show both the name and value. If you just
want a list of names add parameter 'names'.
EOH
    SHORT_HELP   = 'Show instance variables of the current stack frame'
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    MIN_ABBREV   = 'iv'.size
    NEED_STACK   = true
  }

    def names
        @proc.debug_eval('self.instance_variables.sort').map{|n| n.to_s}
    end

    def run_for_type(args, type, klass=nil)
        suffix = klass ? " for #{klass.to_s}" : '' rescue ''
        if args.size == 2
            last_arg = args[-1]
            if 0 == '--names'.index(last_arg)
                if names.empty?
                    msg "No #{type} variables defined."
                else
                    section "#{type.capitalize} variable names#{suffix}:"
                    width = settings[:maxwidth]
                    mess = Columnize::columnize(names,
                                                @proc.settings[:maxwidth], '  ',
                                                false, true, ' ' * 2).chomp
                    msg mess
                end
            elsif 'CFUNC' == @proc.frame.type
                msg "Can't show #{type} names in a C call"
                return
            else
                p last_arg
                if names.member?(last_arg)
                    var_value =
                        @proc.safe_rep(@proc.debug_eval_no_errmsg(last_arg).inspect)
                    msg("#{last_arg} = #{var_value}", :code => true)
                else
                    errmsg "No #{type} variable #{last_arg} defined#{suffix}."
                end
            end
        elsif args.size == 1
            if names.empty?
                msg "No #{type} variables defined#{suffix}."
            else
                section "#{type.capitalize} variables#{suffix}:"
                names.each do |var_name|
                    var_value =
                        @proc.safe_rep(@proc.debug_eval_no_errmsg(var_name).inspect)
                    msg("#{var_name} = #{var_value}", :code => true)
                end
            end
        else
            errmsg("Wrong number of arguments #{args.size}")
        end
    end

    def run(args)
        run_for_type(args, 'instance', @proc.debug_eval('self'))
    end
end

# Demo it.
if __FILE__ == $0
    require_relative '../../../mock'
    require_relative '../variables'
    # cmd =
    #     MockDebugger::subsub_setup(Trepan::Subcommand::InfoVariables,
    #                                Trepan::Subcommand::InfoVariablesInstance)
    # cmd.run(cmd.prefix)
    # cmd.run(cmd.prefix + ['name'])
end
