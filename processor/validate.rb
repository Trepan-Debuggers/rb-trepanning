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
        if opts[:min_value] and ret_value < opts[:min_value]
          errmsg("Expecting integer value to be at least %d; got %d.",
                 opts[min_value], ret_value)
          return nil
        elsif opts[:max_value] and ret_value > opts[:max_value]
          errmsg("Expecting integer value to be at most %d; got %d.",
                 opts[:min_value], ret_value)
          return nil
        end
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
      begin
        val = Integer(eval(arg, b))
      rescue SyntaxError
        return nil
      rescue 
        return nil
      end
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

    # parse_position(self, arg)->(fn, name, lineno)
    # 
    # Parse arg as [filename:]lineno | function | module
    # Make sure it works for C:\foo\bar.py:12
    def parse_position(arg, old_mod=nil)
        colon = arg.rindex(':') 
        if colon
          # First handle part before the colon
          arg1 = arg[colon..-1].rstrip
          lineno_str = arg[colon+1..-1].lstrip
          mf, filename, lineno = parse_position_one_arg(arg1, old_mod, false)
          return nil, nil, nil unless filename
          filename = canonic_file(arg1) 
          # Next handle part after the colon
          val = get_an_int(lineno_str, "Bad line number: %s" % 
                           lineno_str)
          lineno = val if val
        else
          mf, filename, lineno = parse_position_one_arg(arg, old_mod, true)
        end

        return mf, filename, lineno
    end

    # parse_position_one_arg(self,arg)->(module/function, file, lineno)
    #
    # See if arg is a line number, function name, or module name.
    # Return what we've found. nil can be returned as a value in
    # the triple.
    def parse_position_one_arg(arg, old_mod=nil, show_errmsg)
      modfunc, filename = nil, nil, nil
      begin
        # First see if argument is an integer
        lineno   = Integer(arg)
        filename = @frame.source_container[1] unless old_mod
      rescue
        msg "Sorry, parse_position_one_arg not complete yet"
        return nil, nil, nil
        begin
          modfunc = debug_eval(arg)
        rescue
          modfunc = arg
        end
        msg = ('Object %s is not known yet as a function, module, or is not found'
               + ' along sys.path, and not a line number.') % arg.to_s
        begin
          # See if argument is a module or function
          if inspect.isfunction(modfunc)
            pass
          elsif inspect.ismodule(modfunc)
            filename = modfunc.__file__
            filename = canonic_file(filename)
            return modfunc, filename, nil
          elsif hasattr(modfunc, 'im_func')
            modfunc = modfunc.im_func
          else
            errmsg(msg) if show_errmsg
            return nil, nil, nil
          end
          code     = modfunc.func_code
          # FIXME:  Can get from iseq. 
          lineno   = 1 # code.co_firstlineno
          filename = @frame_source_container[1]
        rescue
          errmsg(msg) if show_errmsg
          return nil, nil, nil
        end
      end
      return modfunc, canonic_file(filename), lineno
    end
  end
end

if __FILE__ == $0
  # Demo it.
  class Debugger::CmdProcessor
    def errmsg(msg)
      puts msg
    end
  end
  cp = Debugger::CmdProcessor.new
  onoff = %w(1 0 on off)
  onoff.each { |val| puts "onoff(#{val}) = #{cp.get_onoff(val)}" }
  %w(1 1E bad 1+1 -5).each do |val| 
    puts "get_int_noerr(#{val}) = #{cp.get_int_noerr(val).inspect}" 
  end
end
