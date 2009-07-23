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

    def format_stack_entry(frame)
      return 'invalid frame' if frame.invalid?
      # FIXME: prettify 
      s = frame.type + ' '
      s += ' ' + frame.method if frame.method
      s += " #{frame.proc} #{frame.proc.arity}" if frame.proc
      s += " #{frame.source_container} at line #{frame.source_location}"
    end

    def print_stack_entry(frame)
      # FIXME: remove puts. 
      puts format_stack_entry(frame)
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
  def foo
    puts '=' * 10
    print_stack_trace(RubyVM::ThreadFrame.current)
  end
  foo
end
