# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative 'helper'

class Trepan::Subcommand::InfoRegistersEp < Trepan::SubSubcommand
    unless defined?(HELP)
        Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
        HELP = <<EOH
**#{CMD}**

Show the value of the VM local EP pointer. The EP (environment
pointer?) is used find the next enclosing scope.

See also:
---------

`info register sp".`
EOH

        MIN_ABBREV   = 'ep'.size
        MIN_ARGS     = 0
        MAX_ARGS     = 0
        NEED_STACK   = true
        SHORT_HELP   = "Show the value of the VM EP."
  end

    def run(args)
        frame = @proc.frame
        if 'CFUNC' == frame.type
            msg "ep not available for C functions"
        else
            msg "ep=%s" % frame.ep
        end
    end
end

if __FILE__ == $0
  # Demo it.
    require_relative '../../../mock'
    require_relative '../registers'
    cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::InfoRegisters,
                                     Trepan::SubSubcommand::InfoRegistersLfp,
                                     false)
    cmd.run([])
end
