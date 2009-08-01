class Debugger
  class CmdProcessor

    attr_reader   :current_thread
    attr_accessor :frame        # ThreadFrame, current frame
    attr_accessor :frame_index  # frame index in a "where" command
    attr_accessor :top_frame    # top frame of current thread. Since
                                # right now the ThreadFrame method has "prev" 
                                # but no way to move in the other direction.
                                # So we store the top frame. 
    attr_reader   :threads2frames

    def adjust_frame(frame_num, absolute_pos)
      unless absolute_pos
        frame_num += @frame_index
      end

      if frame_num < 0
        errmsg("Adjusting would put us beyond the newest frame.")
        return
      elsif frame_num >= @top_frame.stack_size
        errmsg("Adjusting would put us beyond the oldest frame.")
        return
      end

      frame = @top_frame.prev(frame_num)
      if frame 
        @frame = frame
        @frame_index = frame_num
        print_location
        @list_lineno = nil
      else
        errmsg("Something went wrong getting frame #{frame_num}.")
      end

    end

    # Initializes the thread and frame variables: @frame, @top_frame, 
    # @frame_index, @current_thread, and @threads2frames
    def frame_setup(frame, current_thread)
      @frame_index    = 0
      @current_thread = current_thread
      @frame = @top_frame = frame
      @threads2frames ||= {}
      @threads2frames[@current_thread] = @top_frame
    end

    # Remove access to thread and frame variables
    def frame_teardown
      @top_frame = @frame = @frame_index = @current_thread = nil 
      @threads2frames = {}
    end


  # # The dance we have to do to set debugger frame state to
  # #    `frame', which is in the thread with id `thread_id'. We may
  # #    need to the hide initial debugger frames.
  # def find_and_set_debugged_frame(frame, thread_id)

  #   thread = threading._active[thread_id]
  #   thread_name = thread.getName()
  #   if (!@settings['dbg_pydbgr'] &&
  #       thread_name == Mthread.current_thread_name())
  #     # The frame we came in on ('current_thread_name') is
  #     # the same as the one we want to switch to. In this case
  #     # we need to some debugger frames are in this stack so 
  #     # we need to remove them.
  #     newframe = Mthread.find_debugged_frame(frame)
  #     frame = newframe unless newframe
  #   end
  #   ## FIXME: else: we might be blocked on other threads which are
  #   # about to go into the debugger it not for the fact this one got there
  #   # first. Possibly in the future we want
  #   # to hide the blocks into threading of that locking code as well. 

  #   # Set stack to new frame
  #   @frame, @curindex = Mcmdproc.get_stack(frame, nil, self.proc)
  #   @proc.stack, @proc.curindex = self.stack, self.curindex

  #   # @frame_thread_name = thread_name
  # end
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  class Debugger::CmdProcessor
    def initialize(frame)
      frame_setup(frame, Thread::current)
    end
    def errmsg(msg)
      puts msg
    end
    def print_location
      puts "#{@frame.source_container} #{@frame.source_location[0]}"
    end
  end

  proc = Debugger::CmdProcessor.new(RubyVM::ThreadFrame.current)
  puts "stack size: #{proc.top_frame.stack_size}"
  0.upto(proc.top_frame.stack_size) { |i| proc.adjust_frame(i, true) }
  puts '*' * 10
  proc.adjust_frame(-1, true)
  proc.adjust_frame(0, true)
  puts '*' * 10
  proc.top_frame.stack_size.times { proc.adjust_frame(1, false) }
  proc.adjust_frame(proc.top_frame.stack_size-1, true)
  proc.top_frame.stack_size.times { proc.adjust_frame(-1, false) }
    
end
