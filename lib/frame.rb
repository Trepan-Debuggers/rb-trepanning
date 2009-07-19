class Debugger
  module Frame
    # Return a count of the number of frames we've got
    def count_frames(frame, count_start=0)
      count = -count_start
      while frame do
        count += 1
        frame = frame.prev
      end
      count
    end

    def print_stack_entry(frame)
      puts "#{frame.source_container} at #{frame.source_location}"
      # To be completed...
    end

    # Print `count' frame entries
    def print_stack_trace(frame, count=nil)
      n = count_frames(frame)
      n = [n, count].min if count
      n.downto(1) do |i|
        print_stack_entry(frame)
        frame = frame.prev
      end
    end
    module_function
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  include Debugger::Frame
  p count_frames(RubyVM::ThreadFrame.current)
  print_stack_trace(RubyVM::ThreadFrame.current)
end
