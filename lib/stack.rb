class Debugger
  module Stack
    # Return a count of the number of frames we've got
    def count_frames(frame, count_start=0)
      count = -count-start
      while frame do
        count += 1
        frame = frame.prev
      end
    end

    def print_stack_entry(frame)
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
