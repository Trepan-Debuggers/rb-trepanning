# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'util'

class Trepan

  # Call-Stack frame methods
  module Frame

    DEFAULT_STACK_TRACE_SETTINGS = {
      :basename     => false,   # Basename only for files?
      :maxstack     => 16,      # How many entries to show? nil means all
      :count        => nil,     # How many entries to show? nil means all
      :current_pos  => 0,       # Where are we in the stack?
      :show_pc      => false,   # Show PC offset?
    } unless defined?(DEFAULT_STACK_TRACE_SETTINGS)

    include Trepan::Util

    module_function

    def all_param_names(iseq, delineate=true)
      return '' unless iseq
      params = param_names(iseq, 0, iseq.argc-1, '')
      if iseq.arg_opts > 0
        opt_params = param_names(iseq, iseq.argc,
                                 iseq.argc + iseq.arg_opts-2, '')
        opt_params[0] = "optional: #{opt_params[0]}" if delineate
        params += opt_params
      end
      params += param_names(iseq, iseq.arg_rest, iseq.arg_rest, '*') if
        iseq.arg_rest != -1
      if iseq.arg_post_len > 0
        # Manditory arguments after optional ones - new in Ruby 1.9
        post_params = param_names(iseq, iseq.arg_post_start,
                                  iseq.post_start + iseq.arg_post_len, '')
        post_params[0] = "post: #{post_params[0]}" if delineate
        params += post_params
      end
      params += param_names(iseq, iseq.arg_block, iseq.arg_block, '&') if
        iseq.arg_block != -1

      return params
    end

    def c_params(frame, maxstring=20)
        argc = frame.argc
        # FIXME should figure out why exception is raised.
        begin
            args =
                if 0 == argc
                    ''
                elsif frame
                    1.upto(argc).map do
                    |i|
                    # FIXME: figure out why
                    # Trepan::Frame:Module throws an error
                    safe_repr(frame.sp(argc-i+3).inspect, 10) rescue ''
                    end.join(', ')
                else
                    '??'
                end
            safe_repr(args, maxstring) rescue ''
        rescue NotImplementedError
            '??'
        end
    end

    # Return the eval string. We get this as the
    # parameter to the eval C call. A bit of checking is done
    # to make sure everything is okay:
    #  - we have to be in an EVAL type frame which has an iseq
    #  - we have to have a prev frame which is a CFUNC
    #  - the prev method name has to be 'eval'
    def eval_string(frame)
      return nil unless 'EVAL' == frame.type && frame.iseq
      prev = frame.prev
      return nil unless prev && 'CFUNC' == prev.type && 'eval' == prev.method
      retval = prev.sp 3
      retval = $1 if retval =~ /^\(eval "(.+)"\)/
      retval
    end

    def file
      iseq.source_container[1]
    end

    def format_stack_call(frame, opts)
        # FIXME: prettify
        s = "#{frame.type}"
        # FIXME: Figure why frame.class can throw a NoMethodError
        # on to_s.
        s += if opts[:class]
                 " #{opts[:class]}#"
             else
                 " #{frame.klass}#" rescue ''
             end
        meth = frame.method
        if meth and frame.type != 'IFUNC'
            iseq = frame.iseq
            args = if 'CFUNC' == frame.type
                       c_params(frame)
                   elsif iseq
                       all_param_names(iseq).join(', ')
                   end
            s += meth
            if %w(CFUNC METHOD).member?(frame.type)
                s += "(#{args})"
            elsif %w(BLOCK LAMBDA TOP EVAL).member?(frame.type)
                s += " |#{args}|" unless args.nil? || args.empty?
            else
                s += "(#{all_param_names(iseq)})"
            end
        end
        s
    end

    def format_stack_entry(frame, opts={})
      return 'invalid frame' unless frame.valid?
      s  = format_stack_call(frame, opts)
      s += " in #{frame.source_container[0]} "
      s +=
        if (eval_str = eval_string(frame))
          safe_repr(eval_str.inspect, 15)
        else
          if 'file' == frame.source_container[0] &&
              opts[:basename]
            File.basename(frame.source_container[1])
          else
            frame.source_container[1]
          end
        end
      if frame.source_location
        s +=
          if opts[:maxwidth] && s.size > opts[:maxwidth]
            "\n\t"
          else
            ' '
          end
        if frame.source_location.size == 1
          s += "at line #{frame.source_location[0]}" if
            frame.source_location[0] != 0
        else
          s += " at lines #{frame.source_location}"
        end
      end
      s += ", pc: #{frame.pc_offset}" if
        frame.pc_offset > 0 && opts[:show_pc]
      return s
    end

    # Return true if frame1 and frame2 are at the same place.
    # We use this for example in detecting tail recursion.
    def location_equal(frame1, frame2)
      frame1 && frame2 && frame1.source_location == frame2.source_location &&
        frame1.pc_offset == frame2.pc_offset &&
        frame1.source_container == frame2.source_container
    end

    def offset_for_return(event)
      raise RuntimeError unless %w(return c-return).member?(event)
      # FIXME: C calls have a RubyVM::Env added to the stack.
      # Where? Why?
      'return' == event ? 1 : 4
    end

    def param_names(iseq, start, stop, prefix='')
      start.upto(stop).map do |i|
        begin
          prefix + iseq.local_name(i)
        rescue
          nil
        end
      end.compact
    end

    def print_stack_entry(frame, i, prefix='    ', opts={})
      opts[:class] = nil unless i == 0
      msg "%s%s" % [prefix, format_stack_entry(frame, opts)]
    end

    def print_stack_trace_from_to(from, to, frame, opts)
        last_frame = nil
        # TODO: handle indirect recursion.
        direct_recursion_count = 0
        from.upto(to) do |i|
            if location_equal(last_frame, frame)
                direct_recursion_count += 1
            else
                if direct_recursion_count > 0
                    msg("... above line repeated #{direct_recursion_count} times")
                    direct_recursion_count = 0
                end
                prefix = (i == opts[:current_pos]) ? '-->' : '   '
                prefix += ' #%d ' % [i]
                print_stack_entry(frame, i, prefix, opts)
            end
            last_frame = frame
            frame = frame.prev
        end
        return last_frame
    end

    # Print `count' frame entries
    def print_stack_trace(frame, opts={})
        opts    = DEFAULT_STACK_TRACE_SETTINGS.merge(opts)
        halfstack = (opts[:maxstack]+1) / 2
        n       = frame.stack_size
        n       = [n, opts[:count]].min if opts[:count]
        if n > (halfstack * 2)
            print_stack_trace_from_to(0, halfstack-1, frame, opts)
            msg "... %d levels ..." % (n - halfstack*2)
            last_frame = print_stack_trace_from_to(n - halfstack, n-1, frame, opts)
        else
            last_frame = print_stack_trace_from_to(0, n-1, frame, opts)
        end
        msg "(More stack frames follow...)" if last_frame.type != 'TOP'
    end

    def set_return_value(frame, event, value)
      offset = offset_for_return(event)
      frame.sp_set(offset, value)
    end
    module_function :set_return_value

    def value_returned(frame, event)
      frame.sp(offset_for_return(event))
    end
  end
end

if __FILE__ == $0
    # Demo it.
    require 'thread_frame'
    include Trepan::Frame
    def msg(msg)
        puts msg
    end
    print_stack_trace(RubyVM::Frame.get, :basename => true)
    def foo
        puts '=' * 10
        print_stack_trace(RubyVM::Frame.get, :show_pc => true)
    end
    foo

    def bar(a, b, c)
        puts '=' * 10
        print_stack_trace(RubyVM::Frame.get,
                          )
    end
    bar(1, 2, 3)

    def baz(a, b, c=5)
        puts '=' * 10
        print_stack_trace(RubyVM::Frame.get)
    end
    baz(1, 2)

    def bat(a, b, &block)
        puts '=' * 10
        print_stack_trace(RubyVM::Frame.get)
    end
  bat(1, 2)

    def babe(a, b, *rest)
        puts '=' * 10
        print_stack_trace(RubyVM::Frame.get)
    end
    babe(1, 2)

    puts '=' * 10
    x  = lambda { |a,b|  print_stack_trace(RubyVM::Frame::get) }
    x.call(1,2)
    puts '=' * 10
    x  = Proc.new do |a|
        print_stack_trace(RubyVM::Frame::get)
    end
    x.call(1,2)
    class C # :nodoc
        def initialize(a)
            print_stack_trace(RubyVM::Frame::get)
        end
    end
    puts '=' * 30
    C.new('Hi')
    puts '=' * 30
    eval("print_stack_trace(RubyVM::Frame.get)")
    puts '=' * 30
    eval("eval('print_stack_trace(RubyVM::Frame.get)')")
    puts '=' * 30
    eval("eval('print_stack_trace(RubyVM::Frame.get, :maxstack => 2)')")
    puts '=' * 30
    1.upto(1) do |a; b|
        print_stack_trace(RubyVM::Frame::get)
    end
end
