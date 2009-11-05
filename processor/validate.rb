# Debugger command input validation routines.
# A String is usually passed in.
class Debugger
  class CmdProcessor

    def confirm(msg, default)
      @dbgr.intf[-1].confirm(msg, default)
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

    def object_iseq(object_string)
      if debug_eval_no_errmsg("#{object_string}.respond_to?('iseq')")
        debug_eval_no_errmsg("#{object_string}.iseq")
      else
        parts = object_string.split(/[.]/)
        string = 
          if parts.size < 2 
            "method(\"#{object_string}\").iseq"
          else
            parts[0..-2].join('.')+".method(\"#{parts[-1]}\").iseq"
          end
        debug_eval_no_errmsg(string)
      end
    rescue
      nil
    end

    def breakpoint_position(args)
      first = args.shift
      iseq = object_iseq(first)
      position_str = 
        if iseq
          args.empty? ? 'O0' : args.shift
        else
          iseq = @frame.iseq 
          first
        end
      use_offset = 
        if position_str.size > 0 && position_str[0].downcase == 'o'
          position_str[0] = ''
          true
        else
          false
        end
      opts = {
        :msg_on_error => 
        ("argument '%s' does not seem to eval to a method or an integer." % 
         position_str),
        :min_value => 0
      }
      position = get_an_int(position_str, opts)
      return [position, iseq, use_offset]
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
      return true  if arg == '1' or arg == 'on'
      return false if arg == '0' or arg =='off'

      errmsg("Expecting 'on', 1, 'off', or 0. Got: %s." % arg.to_s) if
        print_error
      raise TypeError
    end

    # parse_position(self, arg)->(fn, container, lineno)
    # 
    # Parse arg as [filename:]lineno | function | module
    # Make sure it works for C:\foo\bar.py:12
    def parse_position(arg, old_mod=nil)
        colon = arg.rindex(':') 
        if colon
          # First handle part before the colon
          arg1 = arg[0...colon].rstrip
          lineno_str = arg[colon+1..-1].lstrip
          mf, container, lineno = parse_position_one_arg(arg1, old_mod, false)
          return nil, nil, nil unless container
          filename = canonic_file(arg1) 
          # Next handle part after the colon
          val = get_an_int(lineno_str)
          lineno = val if val
        else
          mf, container, lineno = parse_position_one_arg(arg, old_mod, true)
        end

        return mf, container, lineno
    end

    # parse_position_one_arg(self,arg)->(module/function, container, lineno)
    #
    # See if arg is a line number, function name, or module name.
    # Return what we've found. nil can be returned as a value in
    # the triple.
    def parse_position_one_arg(arg, old_mod=nil, show_errmsg)
      modfunc, filename = nil, nil, nil
      begin
        # First see if argument is an integer
        lineno   = Integer(arg)
      rescue
      else
        container = frame_container(@frame, false)
        filename = container[1] unless old_mod
        return nil, [container[0], canonic_file(filename)], lineno
      end

      # Next see if argument is a file name 
      return nil, [container && container[0], canonic_file(arg)], 1 if 
        LineCache::cached?(arg)

      # How about a method name with an instruction sequence?
      iseq = object_iseq(arg)
      if iseq && iseq.source_container[0] == 'file'
        filename = iseq.source_container[1]
        line_no = iseq.offsetlines.values.flatten.min
        return arg, ['file', canonic_file(filename)], line_no
      end

      errmsg("#{arg} is not a line number, read-in filename or method that we can get location information about")
      return nil, nil, nil
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative %w(.. lib mock)
  require_relative %w(main) # Have to include before defining CmdProcessor!
                            # FIXME

  proc = Debugger::CmdProcessor.new(Debugger::MockCore.new())
  proc.frame_setup(RubyVM::ThreadFrame.current)
  onoff = %w(1 0 on off)
  onoff.each { |val| puts "onoff(#{val}) = #{proc.get_onoff(val)}" }
  %w(1 1E bad 1+1 -5).each do |val| 
    puts "get_int_noerr(#{val}) = #{proc.get_int_noerr(val).inspect}" 
  end
  def foo; 5 end
  puts proc.object_iseq('food').inspect
  puts proc.object_iseq('foo').inspect
  puts proc.object_iseq('proc.method_iseq').inspect
end
