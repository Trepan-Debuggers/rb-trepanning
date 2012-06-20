# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetAutoEval < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP = "Evaluate unrecognized debugger commands.

Often inside the debugger, one would like to be able to run arbitrary
Ruby commands without having to preface Python expressions with \"print\" or
\"eval\". Setting \"auto eval\" on will cause unrecognized debugger
commands to be eval'd as a Ruby expression. 

Note that if this is set, on error the message shown on type a bad
debugger command changes from:

  Undefined command: \"fdafds\". Try \"help\".

to something more Ruby-eval-specific such as:

  NameError: name 'fdafds' is not defined

One other thing that trips people up is when setting auto eval is that
there are some short debugger commands that sometimes one wants to use
as a variable, such as in an assignment statement. For example:

  s = 5

which produce when 'auto eval' is on:
  *** Command 'step' can take at most 1 argument(s); got 2.

because by default, 's' is an alias for the debugger 'step'
command. It is possible to remove that alias if this causes constant
problem. Another possibility is to go into a real Ruby shell via the
'irb' command.
"
    MIN_ABBREV = 'ev'.size
    SHORT_HELP = "Set evaluation of unrecognized debugger commands"
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../auto'
  dbgr, cmd = MockDebugger::setup('set')
  cmds = dbgr.core.processor.commands
  set_cmd = cmds['set']
  auto_cmd = Trepan::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                               set_cmd)
  eval_cmd = Trepan::SubSubcommand::SetAutoEval.new(cmd, auto_cmd, 'eval');
  %w(off on 0 1).each { |arg| eval_cmd.run([eval_cmd.name, arg]) }
  puts '-' * 10
  puts eval_cmd.save_command.join("\n")
end
