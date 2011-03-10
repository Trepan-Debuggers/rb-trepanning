# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Trepan command input validation routines.  A String type is
# usually passed in as the argument to validation routines.

require_relative '../app/condition'
require_relative '../app/file'
require_relative '../app/cmd_parse'
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
        if opts[:cmdname]
          errmsg(("Command '%s' expects an integer at least" +
                  ' %d; got: %d.') %
                 [opts[:cmdname], opts[:min_value], opts[:default]])
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

    def position_to_line_and_offset(iseq, filename, position, offset_type)
      case offset_type
      when :line
        if ary = iseq.lineoffsets[position]
          # Normally the first offset is a trace instruction and doesn't
          # register as the given line, so we need to take the next instruction
          # after the first one, when available.
          vm_offset = ary.size > 1 ? ary[1] : ary[0]
          line_no   = position
        elsif found_iseq = find_iseqs_with_lineno(filename, position)
          return position_to_line_and_offset(found_iseq, filename, position, 
                                             offset_type)
        elsif found_iseq = find_iseq_with_line_from_iseq(iseq, position)
          return position_to_line_and_offset(found_iseq, filename, position, 
                                             offset_type)
        else
          errmsg("Unable to find offset for line #{position}\n\t" + 
                 "in #{iseq.name} of file #{filename}")
          return [nil, nil]
        end
      when :offset
        if ary=iseq.offset2lines(position)
          line_no   = ary.first
          vm_offset = position
        else
          errmsg "Unable to find line for offset #{position} in #{iseq}"
          return [nil, nil]
        end
      when nil
        vm_offset = 0
        line_no   = iseq.offset2lines(vm_offset).first
      else
        errmsg "Bad parse offset_type: #{offset_type.inspect}"
        return [nil, nil]
      end
      return [iseq, line_no, vm_offset]
    end

    # Parse a breakpoint position. On success return:
    #   - the instruction sequence to use
    #   - the line number - a Fixnum
    #   - vm_offset       - a Fixnum
    #   - the condition (by default 'true') to use for this breakpoint
    #   - true condition should be negated. Used in *condition* if/unless
    def breakpoint_position(position_str, allow_condition)
      break_cmd_parse = if allow_condition
                          parse_breakpoint(position_str)
                        else
                          parse_breakpoint_no_condition(position_str)
                        end
      return [nil] * 5 unless break_cmd_parse
      tail = [break_cmd_parse.condition, break_cmd_parse.negate]
      meth_or_frame, file, position, offset_type = 
        parse_position(break_cmd_parse.position)
      if meth_or_frame
        if iseq = meth_or_frame.iseq
          iseq, line_no, vm_offset = 
            position_to_line_and_offset(iseq, file, position, offset_type)
          if vm_offset && line_no
            return [iseq, line_no, vm_offset] + tail
          end
        else
          errmsg("Unable to set breakpoint in #{meth_or_frame}")
        end
      elsif file && position
        if :line == offset_type
          iseq = find_iseqs_with_lineno(file, position)
          if iseq
            junk, line_no, vm_offset = 
              position_to_line_and_offset(iseq, file, position, offset_type)
            return [@frame.iseq, line_no, vm_offset] + tail
          else
            errmsg("Unable to find instruction sequence for" + 
                   " position #{position} in #{file}")
          end
        else
          errmsg "Come back later..."
        end
      elsif @frame.iseq.source_container[1] == file 
        line_no, vm_offset = position_to_line_and_offset(@frame.iseq, position, 
                                                         offset_type)
        return [@frame.iseq, line_no, vm_offset] + tail
      else
        errmsg("Unable to parse breakpoint position #{position_str}")
      end
      return [nil] * 5
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

    def get_method(meth)
      start_binding = 
        begin
          @frame.binding
        rescue
          binding
        end
      if meth.kind_of?(String)
        meth_for_string(meth, start_binding)
      else
        begin
          meth_for_parse_struct(meth, start_binding)
        rescue NameError
          errmsg("Can't evalution #{meth.name} to get a method")
          return nil
        end
      end
    end

    # FIXME: this is a ? method but we return 
    # the method value. 
    def method?(meth)
      get_method(meth)
    end

    # parse_position(self, arg)->(meth, filename, offset, offset_type)
    # See app/cmd_parser.kpeg for the syntax of a position which
    # should include things like:
    # Parse arg as [filename:]lineno | function | module
    # Make sure it works for C:\foo\bar.py:12
    def parse_position(info)
      info = parse_location(info) if info.kind_of?(String)
      case info.container_type
      when :fn
        if meth = method?(info.container)
          return [meth, meth.iseq.source_container[1], info.position, 
                  info.position_type]
        else
          return [nil] * 4
        end
      when :file
        filename = canonic_file(info.container)
        # ?? Try to look up method here? 
        return nil, info.container,  info.position, info.position_type
      when nil
        if [:line, :offset].member?(info.position_type)
          container = frame_container(@frame, false)
          filename  = container[1]
          return @frame, canonic_file(filename), info.position, info.position_type
        elsif !info.position_type
          errmsg "Can't parse #{arg} as a position"
          return [nil] * 4
        else
          errmsg "Unknown position type #{info.position_type} for location #{arg}"
          return [nil]  * 4
        end
      else
        errmsg "Unknown container type #{info.container_type} for location #{arg}"
        return [nil] * 4
      end
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

    cmdproc = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    cmdproc.frame_initialize
    cmdproc.instance_variable_set('@settings', 
                               Trepan::CmdProcessor::DEFAULT_SETTINGS)
    cmdproc.frame_setup(RubyVM::ThreadFrame.current)
    onoff = %w(1 0 on off)
    onoff.each { |val| puts "onoff(#{val}) = #{cmdproc.get_onoff(val)}" }
    %w(1 1E bad 1+1 -5).each do |val| 
      puts "get_int_noerr(#{val}) = #{cmdproc.get_int_noerr(val).inspect}" 
    end
    def foo; 5 end
    def cmdproc.errmsg(msg)
      puts msg
    end
    # puts cmdproc.object_iseq('food').inspect
    # puts cmdproc.object_iseq('foo').inspect

    # puts cmdproc.object_iseq('foo@validate.rb').inspect
    # puts cmdproc.object_iseq('cmdproc.object_iseq').inspect
    
    puts cmdproc.parse_position(__FILE__).inspect
    puts cmdproc.parse_position('@8').inspect
    puts cmdproc.parse_position('8').inspect
    puts cmdproc.parse_position("#{__FILE__} #{__LINE__}").inspect

    puts '=' * 40
    ['Array.map', 'Trepan::CmdProcessor.new',
     'foo', 'cmdproc.errmsg'].each do |str|
      puts "#{str} should be method: #{!!cmdproc.method?(str)}"
    end
    puts '=' * 40

    # FIXME:
    puts "Trepan::CmdProcessor.allocate is: #{cmdproc.get_method('Trepan::CmdProcessor.allocate')}"

    ['food', '.errmsg'].each do |str|
      puts "#{str} should be false: #{cmdproc.method?(str).to_s}"
    end
    puts '-' * 20
    p cmdproc.breakpoint_position('foo', true)
    p cmdproc.breakpoint_position('@0', true)
    p cmdproc.breakpoint_position("#{__LINE__}", true)
    p cmdproc.breakpoint_position("#{__FILE__}   @0", false)
    p cmdproc.breakpoint_position("#{__FILE__}:#{__LINE__}", true)
    p cmdproc.breakpoint_position("#{__FILE__} #{__LINE__} if 1 == a", true)
    p cmdproc.breakpoint_position("cmdproc.errmsg", false)
    p cmdproc.breakpoint_position("cmdproc.errmsg:@0", false)
    ### p cmdproc.breakpoint_position(%w(2 if a > b))
    p cmdproc.get_int_list(%w(1+0 3-1 3))
    p cmdproc.get_int_list(%w(a 2 3))
  end
end
