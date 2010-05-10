# -*- coding: utf-8 -*-
require_relative '../base/subcmd'
require_relative '../../../app/thread'
require_relative '../../../app/frame'

class Debugger::Subcommand::InfoThread < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show frame(s) for threads'
    MIN_ABBREV   = 'thr'.size 
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info thread)
  end

  include Debugger::ThreadHelper
  include Debugger::Frame # To show stack

  def get_frame_from_thread(th)
    if th == Thread.current
      @proc.frame
    else
      # FIXME: Check to see if we are blocked on entry to debugger.
      # If so, walk back frames.
      th.threadframe
    end
  end

  def list_threads(verbose=false)
    Thread.list.each_with_index do |th, i|
      main_str = 
        if th == Thread.main
          '(main thread) '
          else
          ''
        end
      frame, line_no, mark = 
        if th == Thread.current
          [@proc.frame, @proc.frame_line, '+']
        else
          [th.threadframe, 
           (@proc.frame.source_location && @proc.frame.source_location[0]),
           ' ']
        end
      frame_info = format_stack_call(frame, {})

      source_container = @proc.frame_container(frame, false)
      loc = @proc.source_location_info(source_container, line_no, frame)

      msg("%s %2d %s%d %s\n\t%s\n\t%s" % 
          [mark, i, main_str, th.object_id, th.inspect, frame_info, loc])
    end
  end

  def run(args)
    if args.size == 2
      list_threads
    elsif args.size > 2
      args[2..-1].each do |id_or_num_str|
        num = @proc.get_int_noerr(id_or_num_str)
        if num
          th = get_thread(num)
          if th
            frame = get_frame_from_thread(th)
            print_stack_trace(frame, {})
          end
        end
      end
    else
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
  puts '-' * 20
  Thread.new do 
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    cmd.run(cmd_args)
    puts '-' * 10
    cmd.run(cmd_args + [0])
    puts '-' * 10
    cmd.run(cmd_args + [1])
  end.join
end
