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
      # FIXME: frame.method_class is not always correct. Investigate.
      s = "#{frame.type} "
      s += "#{frame.method_class}#" if frame.method_class
      if frame.method 
        iseq = frame.iseq
        if frame.type != 'CFUNC'
          args = 0.upto(iseq.arity-1).map do |i| 
            iseq.local_name(i)
          end.join(', ')
        else
          args = "(#{frame.arity} args)"
        end
        s += frame.method
        if frame.type == 'METHOD'
          s += "(#{args})"
        elsif ['BLOCK', 'METHOD', 'LAMBDA', 'TOP', 'EVAL'].member?(frame.type)
          s += " |#{args}|" unless args.empty?
        else
          s += "(#{frame.arity} args)" unless frame.arity == 0
        end
      end
      s += " #{frame.source_container}"
      if frame.source_location
        if frame.source_location.size == 1
          s += " at line #{frame.source_location[0]}" if 
            frame.source_location[0] != 0
        else
          s += " at lines #{frame.source_location}"
        end
      end
      return s
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

  puts '=' * 10
  x  = lambda { |x,y|  print_stack_trace(RubyVM::ThreadFrame::current) }
  x.call(1,2)
  puts '=' * 10
  x  = Proc.new do |a| 
    print_stack_trace(RubyVM::ThreadFrame::current)
  end
  x.call(1,2)
  class C
    def initialize(a)
      print_stack_trace(RubyVM::ThreadFrame::current)
    end
  end
  puts '=' * 30
  C.new('Hi')
  puts '=' * 30
  eval("print_stack_trace(RubyVM::ThreadFrame.current)")
  puts '=' * 30
  [1].each do |a; b|
    p RubyVM::ThreadFrame::current.arity
    print_stack_trace(RubyVM::ThreadFrame::current)
    end      
end
