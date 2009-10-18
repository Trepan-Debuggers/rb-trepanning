# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)

class Debugger::Subcommand::SetAutoEval < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
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
    NAME       = File.basename(__FILE__, '.rb')
    PREFIX     = %w(set auto eval)
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  auto_cmd      = Debugger::SubSubcommand::SetAuto.new(dbgr.core.processor, 
                                                        set_cmd)

  # FIXME: remove the 'join' below
  cmd_name      = Debugger::Subcommand::SetAutoEval::PREFIX.join('')
  autox_cmd     = Debugger::SubSubcommand::SetAutoEval.new(set_cmd.proc, 
                                                           auto_cmd,
                                                           cmd_name)
  # require_relative %w(.. .. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  autox_cmd.run([])
  autox_cmd.run(['off'])
  autox_cmd.run(['on'])

end
