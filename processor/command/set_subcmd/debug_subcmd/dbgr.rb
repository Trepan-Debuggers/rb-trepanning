# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetDebugDbgr < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    NAME        = File.basename(__FILE__, '.rb')
    PREFIX      = %W(set debug #{NAME})
    HELP        = "#{PREFIX.join(" ")} [on|off]

Facilitates debugging the debugger. Global variables $trepan_cmdproc
and $trepan_frame are set to the current values of @frame and self
when the command processor was entered."

    MIN_ABBREV  = 'db'.size
    SHORT_HELP  = 'Set debugging debugger'
  end

  def run(args)
    super
    @proc.cmdloop_prehooks.insert_if_new(-1, *@proc.debug_dbgr_hook)
    @proc.debug_dbgr_hook[1].call
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../debug'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetDebug,
                                   Trepan::SubSubcommand::SetDebugDbgr)
  %w(off on 0 1).each { |arg| cmd.run([cmd.name, arg]) }
  puts '-' * 10
  puts cmd.save_command.join("\n")
end
