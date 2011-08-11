# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'linecache'
require_relative '../app/complete'
require_relative '../app/frame'
require_relative 'virtual'
class Trepan
  class CmdProcessor < VirtualCmdProcessor

    attr_reader   :current_thread

    # ThreadFrame, current frame
    attr_accessor :frame

    # frame index in a "backtrace" command
    attr_accessor :frame_index
    attr_accessor :hide_level

    # Hash[thread_id] -> FixNum, the level of the last frame to
    # show. If we called the debugger directly, then there is
    # generally a portion of a backtrace we don't want to show. We
    # don't need to store this for all threads, just those we want to
    # hide frame on. A value of 1 means to hide just the oldest
    # level. The default or showing all levels is 0.
    attr_accessor :hidelevels      

    # Hash[container] -> file container. This gives us a way to map non-file
    # container objects to a file container for display.
    attr_accessor :remap_container

    # Hash[iseq] -> file container. This gives as a way to map instruction
    # sequences to a file container for display.
    attr_accessor :remap_iseq      

    # top frame of current thread. Since right now the ThreadFrame
    # method has "prev" but no way to move in the other direction.  So
    # we store the top frame.
    attr_accessor :top_frame 

    # Hash[thread_id] -> top_frame
    attr_reader   :threads2frames
    

    def adjust_frame(frame_num, absolute_pos)
      frame, frame_num = get_frame(frame_num, absolute_pos)
      if frame 
        @frame = frame
        @frame_index = frame_num
        unless @settings[:traceprint]
          opts = {
            :basename    => settings[:basename],
            :current_pos => frame_num,
            :maxwidth    => settings[:maxwidth],
          }
          print_stack_trace_from_to(frame_num, frame_num, frame, opts)
          print_location 
        end
        @line_no = frame_line() - 1
        @frame
      else
        nil
      end
    end

    def frame_low_high(direction)
      stack_size = @top_frame.stack_size - @hide_level
      if direction
        low, high = [ @frame_index * -direction, 
                      (stack_size - 1 - @frame_index) * direction ]
        low, high = [high, low] if direction < 0
        [low, high]
      else
        [-stack_size, stack_size-1]
      end
    end
    
    def frame_complete(prefix, direction)
      low, high = frame_low_high(direction)
      ary = (low..high).map{|i| i.to_s}
      Trepan::Complete.complete_token(ary, prefix)
    end

    def frame_container(frame, canonicalize=true)
      container = 
        if @remap_container.member?(frame.source_container)
          @remap_container[frame.source_container]
        elsif frame.iseq && @remap_iseq.member?(frame.iseq.sha1)
          @remap_iseq[frame.iseq.sha1]
        else
          frame.source_container
        end

      container[1] = canonic_file(container[1]) if canonicalize
      container
    end

    def frame_line
      if @event == 'vm-insn' && @frame.iseq
        pc_offset = @frame.pc_offset
        return 0 unless pc_offset
        @frame.iseq.offset2lines(pc_offset)[0]
      else
        (@frame.source_location && @frame.source_location[0]) || 0
      end
    end

    # Initializes the thread and frame variables: @frame, @top_frame, 
    # @frame_index, @current_thread, and @threads2frames
    def frame_setup(frame_thread)
      @frame_index        = 0
      @frame = @top_frame = frame_thread
      @current_thread     = @frame.thread
      @stack_size         = @frame.stack_size

      @threads2frames   ||= {} 
      @threads2frames[@current_thread] = @top_frame
      @hide_level         = 
        if @settings[:debugstack]
          0
        else
          @hidelevels[@current_thread]
        end
      
    end

    # Remove access to thread and frame variables
    def frame_teardown
      @top_frame = @frame = @frame_index = @current_thread = nil 
      @threads2frames = {}
    end

    def frame_initialize
      @remap_container = {}
      @remap_iseq      = {}
      @hidelevels      = Hash.new(0) # Set default value to 0
      @hide_level      = 0
    end

    def get_frame(frame_num, absolute_pos)
      stack_size = @top_frame.stack_size - @hide_level

      if absolute_pos
        frame_num += stack_size if frame_num < 0
      else
        frame_num += @frame_index
      end

      if frame_num < 0
        errmsg('Adjusting would put us beyond the newest frame.')
        return [nil, nil]
      elsif frame_num >= stack_size
        errmsg('Adjusting would put us beyond the oldest frame.')
        return [nil, nil]
      end

      frame = @top_frame.prev(frame_num)
      while 'IFUNC' == frame.type && frame.prev
        frame = frame.prev
        frame_num += 1
      end

      [frame, frame_num]
    end

    def get_nonsync_frame(tf)
      if (tf.stack_size > 10)
        check_frames = (0..5).map{|i| tf.prev(i).method}
        if check_frames == 
            %w(synchronize event_processor IFUNC call trace_hook IFUNC)
          return tf.prev(6)
        end
      end
      tf
    end

    def get_frame_from_thread(th)
      if th == Thread.current
        @threads2frames[th]
      else
        # FIXME: Check to see if we are blocked on entry to debugger.
        # If so, walk back frames.
        tf = get_nonsync_frame(th.threadframe)
        @threads2frames = tf
      end
    end

    # The dance we have to do to set debugger frame state to
    #    `frame', which is in the thread with id `thread_id'. We may
    #    need to the hide initial debugger frames.
    def find_and_set_debugged_frame(th, position)
      
      thread = threading._active[thread_id]
      thread_name = thread.getName()
      if (!@settings['dbg_pydbgr'] &&
          thread_name == Mthread.current_thread_name())
        # The frame we came in on ('current_thread_name') is
        # the same as the one we want to switch to. In this case
        # we need to some debugger frames are in this stack so 
        # we need to remove them.
        newframe = Mthread.find_debugged_frame(frame)
        frame = newframe unless newframe
      end
      ## FIXME: else: we might be blocked on other threads which are
      # about to go into the debugger it not for the fact this one got there
      # first. Possibly in the future we want
      # to hide the blocks into threading of that locking code as well. 
      
      # Set stack to new frame
      @frame, @curindex = Mcmdproc.get_stack(frame, nil, self.proc)
      @proc.stack, @proc.curindex = self.stack, self.curindex
      
      # @frame_thread_name = thread_name
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative '../app/mock'


  class Trepan::CmdProcessor
    def errmsg(msg)
      puts msg
    end
    def print_location
      puts "#{@frame.source_container} #{frame.source_location[0]}"
    end
  end

  proc = Trepan::CmdProcessor.new(Trepan::MockCore.new())
  proc.frame_setup(RubyVM::ThreadFrame.current)
  proc.frame_initialize
  puts "stack size: #{proc.top_frame.stack_size}"
  0.upto(proc.top_frame.stack_size) { |i| proc.adjust_frame(i, true) }
  puts '*' * 10
  proc.adjust_frame(-1, true)
  proc.adjust_frame(0, true)
  puts '*' * 10
  proc.top_frame.stack_size.times { proc.adjust_frame(1, false) }
  puts '*' * 10
  proc.adjust_frame(proc.top_frame.stack_size-1, true)
  proc.top_frame.stack_size.times { proc.adjust_frame(-1, false) }
    
end
