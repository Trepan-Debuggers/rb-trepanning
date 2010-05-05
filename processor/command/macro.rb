# -*- coding: utf-8 -*-
require_relative 'base/cmd'
require_relative '../eval'
class Debugger::Command::MacroCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
      "macro NAME PROC-OBJECT

Define NAME as a debugger macro. Debugger macros get a list of arguments
and should return a command string to use in its place.

Here is a contrived example that shows how to issue 'finish' if we are in 
method 'gcd' and 'step' otherwise:

  macro step_unless_gcd Proc.new{|*args| \"$rbdbgr_frame.method == 'gcd' ? 'finish' : 'step'\"}

Here is another real example. I use the following can be used debug a
debugger command, assuming 'set debug dbgr' is set:

  macro dbgcmd Proc.new{|*args| \"debug $rbdbgr_cmdproc.commands['\#{args[0]}'].run(\#{args.inspect})\"}

With the above, 'dbgcmd list 5' will debug the debugger 'list' command 
on the command 'list 5'.
"

    CATEGORY      = 'support'
    MIN_ARGS      = 2  # Need at least this many
    NAME          = File.basename(__FILE__, '.rb')
    SHORT_HELP    = 'Define a macro'
  end
  
  def run(args)
    macro_name = args[1]
    proc_argstr = @proc.cmd_argstr[macro_name.size..-1].lstrip
    proc_obj = @proc.debug_eval(proc_argstr, @proc.settings[:maxstring])
    if proc_obj
      if proc_obj.is_a?(Proc)
        @proc.macros[macro_name] = proc_obj
        msg "Macro #{macro_name} defined"
      else
        errmsg "Expecting a Proc object; got: #{proc_argstr}"
      end
    end
  end
end
        
if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmdproc = dbgr.core.processor
  ['macro foo Proc.new{|x, y| x+y}',
   'macro bad x+1',
   'macro bad2 1+2'].each do |cmdline|
    args = cmdline.split
    cmd_argstr = cmdline[args[0].size..-1].lstrip
    cmdproc.instance_variable_set('@cmd_argstr', cmd_argstr)
    cmd.run(args)
  end
  p cmdproc.macros
end
