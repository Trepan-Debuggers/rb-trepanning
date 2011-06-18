# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::InfoVariables < Trepan::SubSubcommandMgr
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
#{CMD} [locals|globals|instance]

List various classes of variables for the current stack frame.

Examples:
#{CMD} locals    # show local variables
#{CMD} globals   # show global variables
    EOH
    NEED_STACK   = true
    SHORT_HELP   = 'List names and/or values from the current stack frame'
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr, parent_cmd = MockDebugger::setup('info', false)
  cmd = Trepan::SubSubcommand::InfoVariables.new(dbgr.core.processor, 
                                                 parent_cmd)
  cmd.run(cmd.prefix + %w(locals))
  cmd.run(cmd.prefix + %w(globals name))
  %w(loc glo globals i).each do |prefix|
    p [prefix, cmd.complete(prefix)]
    end
  end
