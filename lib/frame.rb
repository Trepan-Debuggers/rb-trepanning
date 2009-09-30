class Debugger
  module Frame
    def param_names(iseq, start, stop, prefix='')
      start.upto(stop).map do |i| 
        begin
          prefix + iseq.local_name(i)
        rescue
          nil
        end
      end.compact
    end

    def all_param_names(iseq)
      return '' unless iseq
      params = param_names(iseq, 0, iseq.argc-1, '')
      if iseq.arg_opts > 0
        opt_params = param_names(iseq, iseq.argc, 
                                 iseq.argc + iseq.arg_opts-1, '')
        opt_params[0] = "optional: #{opt_params[0]}"
        params += opt_params
      end
      params += param_names(iseq, iseq.arg_rest, iseq.arg_rest, '*') if 
        iseq.arg_rest != -1
      params += param_names(iseq, iseq.arg_block, iseq.arg_block, '&') if 
        iseq.arg_block != -1
      return params.join(', ')
    end

    def format_stack_entry(frame, opts={})
      return 'invalid frame' if frame.invalid?
      # FIXME: prettify 
      s = "#{frame.type} "
      s += "#{eval('self.class', frame.binding)}#" 
      if frame.method and frame.type != 'IFUNC'
        iseq = frame.iseq
        args = if 'CFUNC' == frame.type
                 "(#{frame.arity} args)"
               else
                 all_param_names(iseq)
               end
        s += frame.method
        if frame.type == 'METHOD'
          s += "(#{args})"
        elsif ['BLOCK', 'METHOD', 'LAMBDA', 'TOP', 'EVAL'].member?(frame.type)
          s += " |#{args}|" unless args.empty?
        else
          # FIXME: handle post_len post_start
          s += "(#{all_param_names(iseq)})" 
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

  def bar(a, b, c)
    puts '=' * 10
    print_stack_trace(RubyVM::ThreadFrame.current)
  end
  bar(1, 2, 3)

  def baz(a, b, c=5)
    puts '=' * 10
    print_stack_trace(RubyVM::ThreadFrame.current)
  end
  baz(1, 2)

  def bat(a, b, &block)
    puts '=' * 10
    print_stack_trace(RubyVM::ThreadFrame.current)
  end
  bat(1, 2)

  def babe(a, b, *rest)
    puts '=' * 10
    print_stack_trace(RubyVM::ThreadFrame.current)
  end
  babe(1, 2)

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
