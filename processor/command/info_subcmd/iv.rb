# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'locals'

class Trepan::Subcommand::InfoIv < Trepan::Subcommand::InfoLocals
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
    run_for_type(args, 'instance')
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoIv, false)
  cmd.run(cmd.prefix)
  cmd.run(cmd.prefix + ['name'])
end
