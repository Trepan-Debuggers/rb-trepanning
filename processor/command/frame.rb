require_relative 'base_cmd'
require_relative %w(.. .. lib frame)

class Debugger::Command::FrameCommand < Debugger::Command

  include Debugger::Frame

  unless defined?(HELP)
    HELP = 
"frame [thread-Name] [frame-number]
    
Change the current frame to frame `frame-number' if specified, or the
current frame, 0, if no frame number specified.

If a thread name or thread number is given, change the current frame
to a frame in that thread. Dot (.) can be used to indicate the name of
the current frame the debugger is stopped in.

A negative number indicates the position from the other or 
least-recently-entered end.  So 'frame -1' moves to the oldest frame,
and 'frame 0' moves to the newest frame. Any variable or expression
that evaluates to a number can be used as a position, however due to
parsing limitations, the position expression has to be seen as a single
blank-delimited parameter. That is, the expression '(5*3)-1' is okay
while '( (5 * 3) - 1 )' isn't.

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
  
  # The simple case: thread frame switching has been done or is
  # not needed and we have an explicit position number as a string
  def one_arg_run(position_str)
    opts={
      :msg_on_error => 
      "The 'frame' command requires a frame number. Got: #{position_str}"
    }
    frame_num = @proc.get_an_int(position_str, opts)
    return false unless frame_num
      
    stack_size = @proc.top_frame.stack_size
    if stack_size == 0
      errmsg('No frames recorded.')
      return false
    end
        
    if frame_num < -stack_size or frame_num > stack_size-1
      errmsg(('Frame number has to be in the range %d to %d.' +
              ' Got: %d (%s).') % [-stack_size, stack_size-1, 
                                   frame_num, position_str])
      return false
    else
      @proc.adjust_frame(frame_num, true)
      return true
    end
  end

  # See if `name_or_id' is either a thread name or a thread id.
  # The frame of that id/name is returned, or None if name_or_id is
  # invalid.
  def get_from_thread_name_or_id(name_or_id, report_error=true)
    # FIXME: for now this is what we do.
    return nil, nil

    # FIXME: the below is a slight port of pydbgr
    thread_id = @proc.get_int_noerr(name_or_id)
    unless thread_id
      # Must be a "frame" command with frame name, not a frame
      # number (or invalid command).
      name2id = map_thread_names()
      if name_or_id == '.'
        name_or_id = current_thread_name()
      end
      thread_id = name2id.get(name_or_id)
      unless thread_id
        errmsg("I don't know about thread name %s." % name_or_id)
        return nil, nil
      end
      # Above we should have set thread_id. Now see if we can
      # find it.
      # FIXME: This is for Python.
      threads   = sys._current_frames()
      frame     = threads.get(thread_id)
      if !frame && report_error
        errmsg("I don't know about thread number %s (%d)." %
               name_or_id, thread_id)
        ## self.info_thread_terse()
        return nil, nil
      end
      return frame, thread_id
    end
  end

  # Run a frame command. This routine is a little complex
  # because we allow a number parameter variations.
  def run(args)
    if args.size == 1
      # Form is: "frame" which means "frame 0"
      position_str = '0'
    elsif args.size == 2
      # Form is: "frame {position | thread}
      name_or_id = args[1]
      frame, thread_id = get_from_thread_name_or_id(name_or_id,
                                                    false)
      if !frame
        # Form should be: frame position
        position_str = name_or_id
      else
        # Form should be: "frame thread" which means
        # "frame thread 0"
        position_str = '0'
        ## FIXME:
        ## @proc.find_and_set_debugged_frame(frame, thread_id)
      end
      # elsif args.size == 3
      #   # Form is: frame <thread> <position>
      #   name_or_id = args[1]
      #   position_str = args[2]
      #   frame, thread_id = get_from_thread_name_or_id(name_or_id)
      #   if !frame
      #     # Error message was given in routine
      #     return
      #     find_and_set_debugged_frame(frame, thread_id)
      #   end
      one_arg_run(position_str)
      return false
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  def sep ; puts '=' * 40 end
  %w(0 1 -2).each {|count| cmd.run([name, count]); sep }
  def foo(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    %w(0 -1).each {|count| cmd.run([name, count]); sep }
  end
  foo(cmd, name)
end
