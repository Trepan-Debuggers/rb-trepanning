class Debugger
  module Frame
    def format_stack_entry(frame, opts={})
      return 'invalid frame' if frame.invalid?
      # FIXME: prettify 
      s = "#{frame.type} "
      s += "#{eval('self.class', frame.binding)}#" 
      if frame.method and frame.type != 'IFUNC'
        iseq = frame.iseq
        if 'CFUNC' == frame.type
          args = "(#{frame.arity} args)"
        else
          args = 0.upto(iseq.arity-1).map do |i| 
            begin
              iseq.local_name(i)
            rescue
              nil
            end
          end.compact.join(', ')
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
      s += " in #{frame.source_container[0]} #{frame.source_container[1]}"
      if frame.source_location
        if frame.source_location.size == 1
          s += " at line #{frame.source_location[0]}" if 
            frame.source_location[0] != 0
        else
          s += " at lines #{frame.source_location}"
        end
      end
      # s += " pc: #{frame.pc_offset}" if frame.pc_offset > 0
      return s
    end

    def print_stack_entry(frame, i, prefix='    ')
      msg "%s%s" % [prefix, format_stack_entry(frame)]
    end

    # Print `count' frame entries
    def print_stack_trace(frame, count=nil, current_pos=0)
      n = frame.stack_size
      n = [n, count].min if count
      0.upto(n-1) do |i|
        prefix = (i == current_pos) ? '-->' : '   '
        prefix += ' #%d ' % [i]
        print_stack_entry(frame, i, prefix)
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
  def msg(msg)
    puts msg
  end
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
    print_stack_trace(RubyVM::ThreadFrame::current)
    end      
end
