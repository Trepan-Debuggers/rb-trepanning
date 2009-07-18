# Return a count of 
class Debugger
  module Stack
    def count_frames(frame, count_start=0)
      count = -count-start
      while frame do
        count += 1
        frame = frame.prev
      end
    end
    module_function
  end
end
