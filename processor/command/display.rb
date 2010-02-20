# -*- coding: utf-8 -*-
require_relative %w(base cmd)

class Debugger::Command::DisplayCommand < Debugger::Command

  unless defined?(HELP)
    HELP = <<EOH
display [format] EXP
 
Print value of expression EXP each time the program stops.  FMT may be
used before EXP and may be one of 'c' for char, 'x' for hex, 'o' for
octal, 'f' for float or 's' for string.

For now, display expressions are only evaluated when in the same
instruction sequence as the frame that was in effect when the display
expression was set.  This is a departure from gdb and we may allow for
more flexibility in the future to specify whether this should be the
case or not.

With no argument, evaluate and display all currently requested
auto-display expressions.  Use "undisplay" to cancel display
requests previously made.
EOH
    
    CATEGORY      = 'data'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Display expressions when entering debugger'
  end
  
  def run(args)

    if args.size == 1
      # Display anything active
      @proc.run_eval_display
    else
      if %w(/c /x /o /f /s).member?(args[1])
        if 2 == args.size
          errmsg("Expecting an expression after the format")
          return
        end
        format = args[1]
        expr   = args[2..-1].join(' ')
      else
        format = nil
        expr = args[1..-1].join(' ')
      end

      dp = @proc.displays.add(@proc.frame, expr, format)
      unless dp
        errmsg('Error evaluating "%s" in the current frame' % expr)
        return
      end
      msg(dp.format(false))
      @proc.cmdloop_prehooks.insert_if_new(5, *@proc.display_hook)
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)

  cmd.run([name])
  cmd.run([name, '/x', '10'])
  cmd.run([name, 'd'])
  print '==' * 10
  cmd.run([name])
end
