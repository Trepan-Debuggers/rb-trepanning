require 'tempfile'
require 'linecache'
require_relative '../app/frame'
class Debugger
  class CmdProcessor

    attr_reader   :current_thread
    attr_accessor :frame           # ThreadFrame, current frame
    attr_accessor :frame_index     # frame index in a "where" command
    attr_accessor :hide_level      # 
    attr_accessor :hidelevels      # Hash[thread_id] -> FixNum, the
                                   # level of the last frame to
                                   # show. If we called the debugger
                                   # directly, then there is generally
                                   # a portion of a backtrace we don't
                                   # want to show. We don't need to
                                   # store this for all threads, just
                                   # those we want to hide frame on. A
                                   # value of 1 means to hide just the
                                   # oldest level. The default or
                                   # showing all levels is 0.
    attr_accessor :remap_container # Hash[container] -> file container
                                   # Gives us a way to map non-file
                                   # container objects to a file
                                   # container for display.
    attr_accessor :remap_iseq      # Hash[iseq] -> file container

    attr_accessor :top_frame       # top frame of current thread. Since
                                   # right now the ThreadFrame method has "prev" 
                                   # but no way to move in the other direction.
                                   # So we store the top frame. 
    attr_reader   :threads2frames  # Hash[thread_id] -> top_frame
    

    def adjust_frame(frame_num, absolute_pos)
      frame, frame_num = get_frame(frame_num, absolute_pos)
      if frame 
        @frame = frame
        @frame_index = frame_num
        frame_eval_remap if 'EVAL' == @frame.type
        print_location
        @line_no = frame_line() - 1
        @frame
      else
        nil
      end
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

    # If frame type is EVAL, set up to remap the string to a temporary file.
    def frame_eval_remap
      to_str = Debugger::Frame::eval_string(@frame)
      return nil unless to_str.is_a?(String)

      # All systems go!
      unless @remap_iseq.member?(@frame.iseq.sha1)
        tempfile = Tempfile.new(['eval-', '.rb'])
        tempfile.open.puts(to_str)

        @remap_iseq[@frame.iseq.sha1] = ['file', tempfile.path]
        tempfile.close
        LineCache::cache(tempfile.path)
      end
      return true
    end

    def frame_line
      if @core.event == 'vm-insn' && @frame.iseq
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

      @threads2frames   ||= {}  # or do we want = {} ? 
      @threads2frames[@current_thread] = @top_frame
      @hide_level         = 
        if @settings[:debugstack]
          0
        else
          @hidelevels[@current_thread]
        end
      
      frame_eval_remap if 'EVAL' == @frame.type
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
  require_relative '../app/mock'
  require_relative 'main'   # Have to include before defining CmdProcessor!
                            # FIXME
  class Debugger::CmdProcessor
    def errmsg(msg)
      puts msg
    end
    def print_location
      puts "#{@frame.source_container} #{frame.source_location[0]}"
    end
  end

  proc = Debugger::CmdProcessor.new(Debugger::MockCore.new())
  proc.frame_setup(RubyVM::ThreadFrame.current)
  proc.hidelevels = {}
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
