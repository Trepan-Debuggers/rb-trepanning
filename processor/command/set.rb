# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/submgr'

class Debugger::Command::SetCommand < Debugger::SubcommandMgr
  unless defined?(HELP)
    HELP =
'Modifies parts of the debugger environment.

You can give unique prefix of the name of a subcommand to get
information about just that subcommand.

Type "set" for a list of "set" subcommands and what they do.
Type "help set *" for just the list of "set" subcommands.

For compatability with older ruby-debug "set auto..." is the
same as "set auto ...". For example "set autolist" is the same 
as "set auto list".
'

    CATEGORY      = 'support'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Modify parts of the debugger environment'
  end

  def run(args)
    if args.size > 1 
      first = args[1].downcase
      alen = 'auto'.size
      args[1..1] = ['auto', first[alen..-1]] if
        first.start_with?('auto') && first.size > alen
    end
    super
  end

end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
  cmd.run([name, 'autolist'])
  cmd.run([name, 'autoeval', 'off'])
  cmd.run([name, 'basename'])
end
