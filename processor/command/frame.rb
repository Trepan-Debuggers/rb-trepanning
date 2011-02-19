# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../../app/frame'

class Trepan::Command::FrameCommand < Trepan::Command

  include Trepan::Frame

  unless defined?(HELP)
    HELP = 
"frame [thread-Name] [frame-number]
    
Change the current frame to frame `frame-number' if specified, or the
most-recent frame, 0, if no frame number specified.

If a thread name or thread number is given, change the current frame
to a frame in that thread. Dot (.) can be used to indicate the name of
the current frame the debugger is stopped in.

A negative number indicates the position from the other or
least-recently-entered end.  So 'frame -1' moves to the oldest frame.
Any variable or expression that evaluates to a number can be used as a
position, however due to parsing limitations, the position expression
has to be seen as a single blank-delimited parameter. That is, the
expression '(5*3)-1' is okay while '( (5 * 3) - 1 )' isn't.

Examples:
   frame     # Set current frame at the current stopping point
   frame 0   # Same as above
   frame 5-5 # Same as above. Note: no spaces allowed in expression 5-5
   frame .   # Same as above. 'current thread' is explicit.
   frame . 0 # Same as above.
   frame 1   # Move to frame 1. Same as: frame 0; up
   frame -1  # The least-recent frame
   frame MainThread 0 # Switch to frame 0 of thread MainThread
   frame MainThread   # Same as above
   frame -2434343 0   # Use a thread number instead of name

See also 'up', 'down' 'where' and 'info thread'.
"

    CATEGORY      = 'stack'
    MAX_ARGS      = 2  # Need at most this many
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = true
    SHORT_HELP    = 'Select and print a stack frame'
  end
  
  def complete(prefix)
    @proc.frame_complete(prefix)
  end
  
  # The simple case: thread frame switching has been done or is
  # not needed and we have an explicit position number as a string
  def one_arg_run(position_str)
    stack_size = @proc.top_frame.stack_size - @proc.hide_level
    opts={
      :msg_on_error => 
      "The '#{NAME}' command requires a frame number. Got: #{position_str}",
      :min_value => -stack_size,
      :max_value => stack_size-1
    }
    frame_num = @proc.get_an_int(position_str, opts)
    return false unless frame_num
      
    # FIXME: move into @proc and test based on NEED_STACK.
    if not @proc.top_frame
      errmsg('No frames recorded.')
      return false
    end
        
    @proc.adjust_frame(frame_num, true)
    return true
  end

  # Run a frame command. This routine is a little complex
  # because we allow a number parameter variations.
  def run(args)
    if args.size == 1
      # Form is: "frame" which means "frame 0"
      position_str = '0'
    elsif args.size == 2
      # Form is: "frame position"
      position_str = args[1]
    elsif args.size == 3
      # Form is: frame <position> <thread> 
      name_or_id = args[1]
      thread_str = args[2]
      th = @proc.get_thread_from_string(thread_str)
      if th
        @proc.frame_setup(th.threadframe)
        return
      else
        # FIXME: Give suitable error message was given
      end
    else
      # Form should be: "frame thread" which means
      # "frame thread 0"
      position_str = '0'
      ## FIXME:
      ## @proc.find_and_set_debugged_frame(frame, thread_id)
    end
    one_arg_run(position_str)
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup

  def sep ; puts '=' * 40 end
  %w(0 1 -2).each {|count| cmd.run([cmd.name, count]); sep }
  def foo(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    %w(0 -1).each {|count| cmd.run([name, count]); sep }
  end
  foo(cmd, cmd.name)
end
