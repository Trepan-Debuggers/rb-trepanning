# -*- coding: utf-8 -*-
require_relative File.join(%w(.. base_subcmd))

class Debugger::Subcommand::SetAutoeval < Debugger::SetBoolSubcommand
  unless defined?(HELP)
    HELP = "Evaluate unrecognized commands.

Often inside the debugger, one would like to be able to run arbitrary
Ruby commands without having to preface Python expressions with \"print\" or
\"eval\". Setting \"autoeval\" on will cause unrecognized debugger
commands to be eval'd as a Ruby expression. 

Note that if this is set, on error the message shown on type a bad
debugger command changes from:

  Undefined command: \"fdafds\". Try \"help\".

to something more Ruby-eval-specific such as:

  NameError: name 'fdafds' is not defined

One other thing that trips people up is when setting autoeval is that
there are some short debugger commands that sometimes one wants to use
as a variable, such as in an assignment statement. For example:

  s = 5

which produce when 'autoeval' is on:
  *** Command 'step' can take at most 1 argument(s); got 2.

because by default, 's' is an alias for the debugger 'step'
command. It is possible to remove that alias if this causes constant
problem. Another possibility is to go into a real Ruby shell via the
'irb' command.
"
    IN_LIST      = true
    MIN_ABBREV   = 'autoe'.size
    NAME          = File.basename(__FILE__, '.rb')
    SHORT_HELP   = 'Evaluate unrecognized debugger commands'
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative File.join(%w(.. .. mock))
  require_relative File.join(%w(.. .. subcmd))
  dbgr = MockDebugger.new
  cmds = dbgr.core.processor.instance_variable_get('@commands')
  cmd = cmds['exit']
  subcommand = Debugger::Subcommand::SetAutoeval.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
