# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'locals'

class Trepan::Subcommand::InfoVariablesGlobals <
    Trepan::Subcommand::InfoVariablesLocals
  Trepan::Util.suppress_warnings {
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD}
#{CMD} [names]

Show global variables.
Normally for each which show both the name and value. If you just
want a list of names add parameter 'names'.
EOH
    SHORT_HELP   = 'Show global variables'
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    NEED_STACK   = true
  }

  def get_names
    global_variables
  end

  def run(args)
    if args.size == 2
      if 0 == 'names'.index(args[-1].downcase)
        names = get_names()
        if names.empty?
            msg "No global variables defined."
        else
          section "Global variable names:"
          width = settings[:maxwidth]
          mess = Columnize::columnize(global_variables.sort, 
                                      @proc.settings[:maxwidth], '  ',
                                      false, true, ' ' * 2).chomp
          msg mess
        end
      else
        errmsg("unrecognized argument: #{args[-1]}")
      end
    elsif args.size == 1
      names = get_names
      if names.empty?
        msg "No global variables defined."
      else
        section "Global variables:"
        names.sort.each do |var_name| 
          s = @proc.debug_eval(var_name.to_s)
          msg("#{var_name} = #{s.inspect}")
        end
      end
    else
      errmsg("Wrong number of arguments #{args.size}")
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  cmd = MockDebugger::subsub_setup(Trepan::Subcommand::InfoVariablesGlobals, 
                                   false)
  cmd.run(cmd.prefix)
  cmd.run(cmd.prefix + ['name'])
end
