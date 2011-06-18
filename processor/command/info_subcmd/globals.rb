# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'columnize'
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Trepan::Subcommand::InfoGlobals < Trepan::Subcommand
  unless defined?(HELP)
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
    MIN_ABBREV   = 'gl'.size 
    NEED_STACK   = true
  end

  def get_names
    global_variables
  end

  def run(args)
    if args.size == 3
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
        errmsg("unrecognized argument #{args[2]}")
      end
    elsif args.size == 2
      names = get_names
      if names.empty?
        msg "No global variables defined."
      else
        section "Global variables:"
        name.sort.each do |var_name| 
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
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoGlobals, false)
  cmd.run(cmd.prefix)
  cmd.run(cmd.prefix + ['name'])
end
