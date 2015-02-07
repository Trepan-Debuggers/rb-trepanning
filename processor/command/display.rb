# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'

class Trepan::Command::DisplayCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
**#{name}** [*format*] *expression*

Print value of expression *expression* each time the program stops;
*format* may be used to specify how to pring and may be one of 'c' for
char, 'x' for hex, 'o' for octal, 'f' for float or 's' for string.

For now, display expressions are only evaluated when in the same
instruction sequence as the frame that was in effect when the display
expression was set.  This is a departure from gdb and we may allow for
more flexibility in the future to specify whether this should be the
case or not.

With no argument, evaluate and display all currently requested
auto-display expressions.  Use `undisplay` to cancel display
requests previously made.

See also:
---------

`undisplay`, `enable`, and `disable`.
    HELP

  CATEGORY      = 'data'
  NEED_STACK    = false
  SHORT_HELP    = 'Display expressions when entering debugger'
end

#def save_command
#  val     = settings[subcmd_setting_key] ? 'on' : 'off'
#  ["#{subcmd_prefix_string} #{val}"]
#end

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
    msg(dp.to_s(@proc.frame))
    @proc.cmdloop_prehooks.insert_if_new(5, *@proc.display_hook)
  end
end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup

  def run_cmd(cmd, args)
    cmd.run(args)
    puts '==' * 10
  end

  cmd.proc.frame_setup(RubyVM::Frame::current)

  run_cmd(cmd, [cmd.name])
  run_cmd(cmd, [cmd.name, '/x', '10'])
  run_cmd(cmd, [cmd.name, 'd'])
  run_cmd(cmd, [cmd.name])
  e = 5
  run_cmd(cmd, [cmd.name, 'e'])
end
