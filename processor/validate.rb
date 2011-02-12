# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Trepan command input validation routines.  A String type is
# usually passed in as the argument to validation routines.

require_relative '../app/condition'
require_relative '../app/file'
require_relative '../app/method_name'
require_relative '../app/thread'

require_relative 'location' # for resolve_file_with_dir
require_relative 'msg'      # for errmsg, msg

class Trepan
  class CmdProcessor

    attr_reader :dbgr_script_iseqs
    attr_reader :dbgr_iseqs

    include Trepanning
    include Trepan::ThreadHelper
    include Trepan::Condition

    def confirm(msg, default)
      @settings[:confirm] ? @dbgr.intf[-1].confirm(msg, default) : true
    end

    # Like cmdfns.get_an_int(), but if there's a stack frame use that
    # in evaluation.
    def get_an_int(arg, opts={})
      ret_value = get_int_noerr(arg)
      if !ret_value
        if opts[:msg_on_error]
          errmsg(opts[:msg_on_error])
        else
          errmsg("Expecting an integer, got: #{arg}.")
        end
        return nil
      end
      if opts[:min_value] and ret_value < opts[:min_value]
        errmsg("Expecting integer value to be at least %d; got %d." %
               [opts[:min_value], ret_value])
        return nil
      elsif opts[:max_value] and ret_value > opts[:max_value]
        errmsg("Expecting integer value to be at most %d; got %d." %
               [opts[:max_value], ret_value])
        return nil
      end
      return ret_value
    end

    unless defined?(DEFAULT_GET_INT_OPTS)
      DEFAULT_GET_INT_OPTS = {
        :min_value => 0, :default => 1, :cmdname => nil, :max_value => nil}
    end

    # If argument parameter 'arg' is not given, then use what is in
    # opts[:default]. If String 'arg' evaluates to an integer between
    # least min_value and at_most, use that. Otherwise report an
    # error.  If there's a stack frame use that for bindings in
    # evaluation.
    def get_int(arg, opts={})
      
      return default unless arg
      opts = DEFAULT_GET_INT_OPTS.merge(opts)
      val = arg ? get_int_noerr(arg) : opts[:default]
      unless val
        if opts[:cmdname]
          errmsg(("Command '%s' expects an integer; " +
                  "got: %s.") % [opts[:cmdname], arg])
        else
          errmsg('Expecting a positive integer, got: %s' % arg)
        end
        return nil
      end
      
      if val < opts[:min_value]
        if cmdname
          errmsg(("Command '%s' expects an integer at least" +
                  ' %d; got: %d.') %
                 [cmdname, opts[:min_value], opts[:default]])
        else
          errmsg(("Expecting a positive integer at least" +
                  ' %d; got: %d') %
                 [opts[:min_value], opts[:default]])
        end
        return nil
      elsif opts[:max_value] and val > opts[:max_value]
        if opts[:cmdname]
          errmsg(("Command '%s' expects an integer at most" +
                  ' %d; got: %d.') %
                 [opts[:cmdname], opts[:max_value], val])
        else
          errmsg(("Expecting an integer at most %d; got: %d") %
                 [opts[:max_value], val])
        end
        return nil
      end
      return val
    end

    def get_int_list(args, opts={})
      args.map{|arg| get_an_int(arg, opts)}.compact
    end
    
    # Eval arg and it is an integer return the value. Otherwise
    # return nil
    def get_int_noerr(arg)
      b = @frame ? @frame.binding : nil
      val = Integer(eval(arg, b))
    rescue SyntaxError
      nil
    rescue 
      nil
    end

    def get_thread_from_string(id_or_num_str)
      if id_or_num_str == '.'
        Thread.current
      elsif id_or_num_str.downcase == 'm'
        Thread.main
      else
        num = get_int_noerr(id_or_num_str)
        if num
          get_thread(num)
        else
          nil
        end
      end
    end

    # Return the instruction sequence associated with string
    # OBJECT_STRING or nil if no instruction sequence
    def object_iseq(object_string)
      iseqs = find_iseqs(ISEQS__, object_string)
      # FIXME: do something if there is more than one.
      if iseqs.size == 1
         iseqs[-1]
      elsif meth = method?(object_string)
        meth.iseq
      else
        nil
      end
    rescue
      nil
    end

    def parse_num_or_offset(position_str)
      err_str = "argument '%s' does not seem to be an integer" % 
        position_str.dup
      use_offset = 
        if position_str.size > 0 && position_str[0].downcase == 'o'
          err_str << 'or an offset.'
          position_str[0] = ''
          true
        else
          err_str << '.'
          false
        end
      opts = {
        :msg_on_error => err_str,
        :min_value => 0 
      }
      position = get_an_int(position_str, opts)
      [position, use_offset]
    end

    # Parse a breakpoint position. Return
    # - the position - a Fixnum
    # - the instruction sequence to use
    # - whether the position is an offset or a line number
    # - the condition (by default 'true') to use for this breakpoint
    def breakpoint_position(args)
      first = args.shift
      name, container, position = parse_position(first, nil, true)
      if container && position
        iseq = find_iseqs_with_lineno(container[1], position) || object_iseq(first)
        unless iseq
          if @frame.iseq && 
              File.basename(@frame.iseq.source_container[1]) == 
              File.basename(container[1])
            iseq = @frame.iseq
          else
            errmsg("Unable to find instruction sequence for" + 
                   " position #{position} in #{container[1]}")
            return [nil, nil, nil, true]
          end
        end
        if args.empty? || 'if' == args[0]
          use_offset = false 
        else
          position, use_offset = parse_num_or_offset(args[0])
        end
      else
        iseq = object_iseq(first)
        position_str = 
          if iseq
            # Got name and possibly position
            name = first
            if args.empty? 
              # FIXME: *Still* have a bug stopping at offset 0.
              # So stop at next offset after 0.
              # 'o0' 
              "o#{@frame.iseq.offsetlines.keys.sort[1]}"
            else
              args.shift
            end
          else
            iseq = @frame.iseq unless container
            first
          end
        position, use_offset = parse_num_or_offset(position_str)
      end
      condition = 'true'
      if args.size > 0 && 'if' == args[0] 
        condition_try = args[1..-1].join(' ')
        condition = condition_try if valid_condition?(condition_try)
      end
      return [position, iseq, use_offset, condition, name]
    end

    # Return true if arg is 'on' or 1 and false arg is 'off' or 0.
    # Any other value is raises TypeError.
    def get_onoff(arg, default=nil, print_error=true)
      unless arg
        if !default
          if print_error
            errmsg("Expecting 'on', 1, 'off', or 0. Got nothing.")
          end
          raise TypeError
        end
        return default
      end
      darg = arg.downcase
      return true  if arg == '1' || darg == 'on'
      return false if arg == '0' || darg =='off'

      errmsg("Expecting 'on', 1, 'off', or 0. Got: %s." % arg.to_s) if
        print_error
      raise TypeError
    end

    include CmdParser

    def get_method(method_string)
      start_binding = 
        begin
          @frame.binding
        rescue
          binding
        end
      meth_for_string(method_string, start_binding)
    end

    # FIXME: this is a ? method but we return 
    # the method value. 
    def method?(method_string)
      begin
        get_method(method_string)
      rescue Citrus::ParseError
        return false
      rescue NameError
        return false
      end
    end

    # parse_position(self, arg)->(fn, container, lineno)
    # 
    # Parse arg as [filename:]lineno | function | module
    # Make sure it works for C:\foo\bar.py:12
    def parse_position(arg, old_mod=nil, allow_offset = false)
      if meth = method?(arg)
        if meth.kind_of?(Method) && iseq = meth.iseq
          line_no = iseq.offsetlines.values.flatten.min
          if iseq.source_container[0] == 'file'
            filename = iseq.source_container[1]
            return arg, ['file', canonic_file(filename)], line_no
          else
            return arg, iseq.container, line_no
          end
        end
      end
      colon = arg.rindex(':') 
      if colon
        # First handle part before the colon
        arg1 = arg[0...colon].rstrip
          lineno_str = arg[colon+1..-1].lstrip
        mf, container, lineno = parse_position_one_arg(arg1, old_mod, false, allow_offset)
        return nil, nil, nil unless container
        filename = canonic_file(arg1) 
        # Next handle part after the colon
        val = get_an_int(lineno_str)
        lineno = val if val
      else
        mf, container, lineno = parse_position_one_arg(arg, old_mod, true, allow_offset)
      end
      
      return mf, container, lineno
    end

    # parse_position_one_arg(self,arg)->(module/function, container, lineno)
    #
    # See if arg is a line number, function name, or module name.
    # Return what we've found. nil can be returned as a value in
    # the triple.
    def parse_position_one_arg(arg, old_mod=nil, show_errmsg=true, allow_offset=false)
      name, filename = nil, nil, nil
      begin
        # First see if argument is an integer
        lineno    = Integer(arg)
      rescue
      else
        container = frame_container(@frame, false)
        filename  = container[1] unless old_mod
        return nil, [container[0], canonic_file(filename)], lineno
      end

      # Next see if argument is a file name 
      found = 
        if arg[0..0] == File::SEPARATOR
          LineCache::cached?(arg)
        else
          resolve_file_with_dir(arg)
        end
      if found
        return nil, [container && container[0], canonic_file(arg)], 1 
      else
        matches = find_scripts(arg)
        if matches.size > 1
          if show_errmsg
            errmsg "#{arg} is matches several files:"
            errmsg Columnize::columnize(matches.sort, 
                                        @settings[:width], ' ' * 4, 
                                        true, true, ' ' * 2).chomp
          end
          return nil, nil, nil
        elsif matches.size == 1
          LineCache::cache(matches[0])
          return nil, ['file', matches[0]], 1
        end
      end

      # How about something with an instruction sequence?
      meth = method?(arg)
      if meth
        if 'instruction sequence' == meth.type
          iseq = meth.iseq
        else
          errmsg("#{meth.class} #{arg} is of type #{meth.type}; " +
                 "it doesn't have an instruction sequence.")
          return meth, nil, nil
        end
      end

      iseq = object_iseq(arg)
      if iseq 
        line_no = iseq.offsetlines.values.flatten.min
        if iseq.source_container[0] == 'file'
          filename = iseq.source_container[1]
          return arg, ['file', canonic_file(filename)], line_no
        else
          return arg, iseq.source_container, line_no
        end
      end

      if show_errmsg
        unless (allow_offset && arg.size > 0 && arg[0].downcase == 'o')
          errmsg("#{arg} is not a line number, read-in filename or method " +
                 "we can get location information about")
        end
      end
      return nil, nil, nil
    end
    
    def validate_initialize
      ## top_srcdir = File.expand_path(File.join(File.dirname(__FILE__), '..'))
      ## @dbgr_script_iseqs, @dbgr_iseqs = filter_scripts(top_srcdir)
    end
  end
end

if __FILE__ == $0
  # Demo it.
  if !(ARGV.size == 1 && ARGV[0] == 'noload')
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    require 'thread_frame'
    require_relative '../app/mock'
    require_relative './default'
    require_relative 'frame'
    require_relative 'main' # Have to include before defining CmdProcessor!
                            # FIXME

    proc = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    proc.frame_initialize
    proc.instance_variable_set('@settings', 
                               Trepan::CmdProcessor::DEFAULT_SETTINGS)
    proc.frame_setup(RubyVM::ThreadFrame.current)
    onoff = %w(1 0 on off)
    onoff.each { |val| puts "onoff(#{val}) = #{proc.get_onoff(val)}" }
    %w(1 1E bad 1+1 -5).each do |val| 
      puts "get_int_noerr(#{val}) = #{proc.get_int_noerr(val).inspect}" 
    end
    def foo; 5 end
    def proc.errmsg(msg)
      puts msg
    end
    puts proc.object_iseq('food').inspect
    puts proc.object_iseq('foo').inspect

    puts proc.object_iseq('foo@validate.rb').inspect
    puts proc.object_iseq('proc.object_iseq').inspect
    
    puts proc.parse_position_one_arg('tmpdir.rb').inspect
    puts proc.parse_position_one_arg('O8').inspect
    puts proc.parse_position_one_arg('8').inspect

    puts '=' * 40
    ['Array.map', 'Trepan::CmdProcessor.new',
     'foo', 'proc.errmsg'].each do |str|
      puts "#{str} should be method: #{proc.method?(str).inspect}"
    end
    puts '=' * 40

    # FIXME:
    puts "Trepan::CmdProcessor.allocate is: #{proc.get_method('Trepan::CmdProcessor.allocate')}"

    ['food', '.errmsg'].each do |str|
      puts "#{str} should be false: #{proc.method?(str).inspect}"
    end
    puts '-' * 20
    p proc.breakpoint_position(%w(O0))
    p proc.breakpoint_position(%w(1))
    p proc.breakpoint_position(%w(2 if a > b))
    p proc.get_int_list(%w(1+0 3-1 3))
    p proc.get_int_list(%w(a 2 3))
  end
end
