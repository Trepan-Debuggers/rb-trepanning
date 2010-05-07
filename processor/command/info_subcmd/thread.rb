# -*- coding: utf-8 -*-
require_relative '../base/subcmd'
require_relative '../../../app/frame' #

class Debugger::Subcommand::InfoThread < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show frame(s) for threads'
    MIN_ABBREV   = 'thr'.size 
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info thread)
  end

  include Debugger::Frame # For format_stack_call
  def run(args)
    Thread.list.each_with_index do |th, i|
      if th == Thread.current
        frame = @proc.frame
        mark = '+'
      else
        # FIXME: check don't show blocked waiting to run hook
        frame = th.threadframe
        mark = ' '
      end
      frame_info = format_stack_call(frame, {})

      # FIXME: print location via routine in location.rb
      msg("%s %2d %d %s\n\t%s" % 
          [mark, i, th.object_id, th.inspect, frame_info])
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  dbgr, cmd  = MockDebugger::setup('info')
  # FIXME: shouldn't have to do this:
  cmd.proc.settings[:maxstring] = 150

  name = File.basename(__FILE__, '.rb')
  cmd_args = ['info', name]

  cmd.proc.instance_variable_set('@last_args', cmd_args)
  cmd_args = ['info', name]
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
  cmd.run(cmd_args)
end
