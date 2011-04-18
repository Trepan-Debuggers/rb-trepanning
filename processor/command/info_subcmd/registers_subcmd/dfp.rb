# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative 'helper'

class Trepan::Subcommand::InfoRegistersDfp < Trepan::SubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Show the value of the VM dynamic frame pointer (DFP)'
    MIN_ABBREV   = 'df'.size
    NEED_STACK   = true
  end

  include Registers
  def run(args)
    register_array_index(PREFIX[-1], args[0])
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../registers'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::InfoRegisters,
                                   Trepan::SubSubcommand::InfoRegistersDfp,
                                   false)
  cmd.run(['0'])
end
