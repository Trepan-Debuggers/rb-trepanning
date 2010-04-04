# -*- coding: utf-8 -*-
require_relative 'base/cmd'

# undisplay display-number...
class Debugger::Command::UndisplayCommand < Debugger::Command
    
  unless defined?(HELP)
    HELP = <<EOH

undisplay DISPLAY_NUMBER ...
Cancel some expressions to be displayed when program stops.
Arguments are the code numbers of the expressions to stop displaying.
No argument means cancel all automatic-display expressions.
"delete display" has the same effect as this command.
Do "info display" to see current list of code numbers.
EOH

    ALIASES       = %w(und)
    CATEGORY      = 'data'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Cancel some expressions to be displayed when program stops'
  end

  def run(args)
    
    if args.size == 1
      @proc.displays.clear
      return
    end
    opts = {}
    args[1..-1].each do |arg|
      opts[:msg_on_error] = '%s must be a display number' % arg
      i = @proc.get_an_int(arg, opts)
      if i 
        unless @proc.displays.delete_index(i)
          errmsg("no display number %d." % i)
          return
        end
      end
      return false
    end
  end
end

if __FILE__ == $0
  # demo it.
  require 'thread_frame'
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  def run_cmd(cmd, args)
    cmd.run(args)
    puts '==' * 10
  end

  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)

  run_cmd(cmd, %w(undisplay z))
  run_cmd(cmd, %w(undisplay 1 10))
end
