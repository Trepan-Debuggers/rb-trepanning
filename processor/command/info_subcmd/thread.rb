# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/thread'
require_relative '../../../app/frame'

class Trepan::Subcommand::InfoThread < Trepan::Subcommand
  unless defined?(HELP)
    HELP         = 
'info thread [THREAD-ARG1 THREAD-ARG2.. ]

Show frame information for thread(s). If no thread arguments are
given, then the top frame information is shown for all threads.

If arguments are given, each number should be either the object id of
the thread, a position number in the list of threads, "M", or ".".  The
object id and list position are given when "info thread" is run.  The
name "M" in upper- or lower- case refers to the "main thread" and "."
refers to the current thread.

Examples:
   info thread             # Show summary frame information
   info thread M           # information for main thread
   info thread .           # information for current thread
   info thread 1 2 m       # information for thread in list 1, 2, and main
   info thread 92562770    # ifnormation for thread with object id 92562770
'
    MIN_ABBREV   = 'thr'.size 
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info thread)
    SHORT_HELP   = 'Show frame(s) for threads'
  end

  include Trepan::ThreadHelper
  include Trepan::Frame # To show stack

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
        th = @proc.get_thread_from_string(id_or_num_str)
        if th
          frame = @proc.get_frame_from_thread(th)
          print_stack_trace(frame, {:maxstack => settings[:maxstack]})
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
