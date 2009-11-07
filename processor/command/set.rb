# -*- coding: utf-8 -*-
require_relative %w(base submgr)

class Debugger::Command::SetCommand < Debugger::SubcommandMgr
  unless defined?(HELP)
    HELP =
'Modifies parts of the debugger environment.

You can give unique prefix of the name of a subcommand to get
information about just that subcommand.

Type "set" for a list of "set" subcommands and what they do.
Type "help set *" for just the list of "set" subcommands.
'

    CATEGORY      = 'data'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Modify parts of the debugger environment'
  end

  def run(args)
    if args.size > 1 && args[1] =~ /^auto./
      args[1..1] = ['auto', args[1][4..-1]]
    end
    super
  end

end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
  cmd.run([name, 'autolist'])
  cmd.run([name, 'autoeval', 'off'])
  cmd.run([name, 'basename'])
end
