require_relative 'util'

include Rbdbgr
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

    def c_params(arity, frame, maxstring=20)
      args = 
        if 0 == arity
          ''
        elsif 0 < arity && frame
          [1..arity].map{|i| frame.sp(i+2).inspect}.join(', ')
        else
          "#{frame.arity} args"
        end
      safe_repr(args, maxstring)
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
      if iseq.arg_post_len > 0
        # Manditory arguments after optional ones - new in Ruby 1.9
        post_params = param_names(iseq, iseq.arg_post_start, 
                                  iseq.post_start + iseq.arg_post_len, '')
        post_params[0] = "post: #{post_params[0]}"
        params += post_params
      end
      params += param_names(iseq, iseq.arg_block, iseq.arg_block, '&') if 
        iseq.arg_block != -1

      return params.join(', ')
    end

    def format_stack_entry(frame, opts={})
      return 'invalid frame' if frame.invalid?
      # FIXME: prettify 
      s = "#{frame.type} "
      s += if opts[:class]
             "#{opts[:class]}#"
           else
             "#{eval('self.class', frame.binding)}#" 
           end
      if frame.method and frame.type != 'IFUNC'
        iseq = frame.iseq
        args = if 'CFUNC' == frame.type
                 c_params(frame.arity, frame)
               else
                 all_param_names(iseq)
               end
        s += frame.method
        if %w(CFUNC METHOD).member?(frame.type)
          s += "(#{args})"
        elsif %w(BLOCK LAMBDA TOP EVAL).member?(frame.type)
          s += " |#{args}|" unless args.empty?
        else
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

    def print_stack_entry(frame, i, prefix='    ', opts={})
      opts = {} unless i == 0
      msg "%s%s" % [prefix, format_stack_entry(frame, opts)]
    end

    # Print `count' frame entries
    def print_stack_trace(frame, count=nil, current_pos=0, opts={})
      n = frame.stack_size
      n = [n, count].min if count
      0.upto(n-1) do |i|
        prefix = (i == current_pos) ? '-->' : '   '
        prefix += ' #%d ' % [i]
        print_stack_entry(frame, i, prefix, opts)
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
  x  = lambda { |a,b|  print_stack_trace(RubyVM::ThreadFrame::current) }
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
  1.times do |a; b|
    print_stack_trace(RubyVM::ThreadFrame::current)
  end
end
