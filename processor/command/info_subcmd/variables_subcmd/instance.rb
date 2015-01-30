# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'locals'

class Trepan::Subcommand::InfoVariablesInstance <
    Trepan::Subcommand::InfoVariablesLocals
  Trepan::Util.suppress_warnings {
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD}
#{CMD} [names]

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

  def get_names
    @proc.debug_eval('self.instance_variables')
  end

  def run(args)
      msg "Not implemented yet"
      return
      run_for_type(args, 'instance', @proc.debug_eval('self'))
  end
end

# Demo it.
if __FILE__ == $0
    require_relative '../../../mock'
    require_relative '../variables'
    cmd =
        MockDebugger::subsub_setup(Trepan::Subcommand::InfoVariables,
                                   Trepan::Subcommand::InfoVariablesInstance)
    cmd.run(cmd.prefix)
    cmd.run(cmd.prefix + ['name'])
end
