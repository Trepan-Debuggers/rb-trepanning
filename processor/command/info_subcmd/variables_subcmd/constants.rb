# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../../../../app/util'

class Trepan::Subcommand::InfoVariablesConstants < Trepan::SubSubcommand
  Trepan::Util.suppress_warnings {
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD}
#{CMD}

Show class constants of the current stack frame.
Normally for each which show both the name and value. If you just
want a list of names add parameter 'names'.
EOH
    SHORT_HELP   = 'Show class constants via the current stack frame'
    MIN_ABBREV   = 'co'.size
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    NEED_STACK   = true
  }

    def names
        # FIXME: do less guess work. Decide based on other properties.
        values = @proc.debug_eval_no_errmsg('self.class.constants')
        if values.empty?
            values = @proc.debug_eval_no_errmsg('self.constants')
        end
        return values ? values.sort.map{|c| c.to_s} || [] : []
    end

    def run_for_constant(args, klass=nil)
        suffix = klass ? " for #{klass.to_s}" : '' rescue ''
        if args.size == 2
            last_arg = args[-1]
            if names.member?(last_arg)
                var_class =
                    @proc.safe_rep(@proc.debug_eval_no_errmsg(last_arg+'.class').inspect)
                var_value =
                    @proc.safe_rep(@proc.debug_eval_no_errmsg(last_arg).inspect)
                msg("#{last_arg} class: %s" % var_class)
                msg("#{last_arg} value: %s" % var_value)
            else
                errmsg "No constant variable #{last_arg} defined#{suffix}."
            end
        elsif args.size == 1
            if names.empty?
                msg "No constant variables defined#{suffix}."
            else
                section "Constant variable names#{suffix}:"
                width = settings[:maxwidth]
                mess = Columnize::columnize(names,
                                            @proc.settings[:maxwidth], '  ',
                                            false, true, ' ' * 2).chomp
                msg mess
            end
        else
            errmsg("Wrong number of arguments #{args.size}")
        end
    end

    def run(args)
        run_for_constant(args, @proc.debug_eval('self'))
    end
end

# Demo it.
if __FILE__ == $0
    require_relative '../../../mock'
    require_relative '../variables'
    # cmd =
    #     MockDebugger::subsub_setup(Trepan::Subcommand::InfoVariables,
    #                                Trepan::Subcommand::InfoVariablesConstants)
    # cmd.run(cmd.prefix)
    # cmd.run(cmd.prefix + ['name'])
end
