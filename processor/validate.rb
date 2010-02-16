# Debugger command input validation routines.
# A String is usually passed in.

require_relative %w(.. app file)
require_relative %w(.. app condition)
class Debugger
  class CmdProcessor
    include Rbdbgr

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
      iseqs = find_iseqs(object_string)
      # FIXME: do something if there is more than one.
      if iseqs.size == 1
         iseqs[-1]
      elsif debug_eval_no_errmsg("#{object_string}.respond_to?('iseq')")
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
          # FIXME: we have trouble stopping at offset 0. 
          if args.empty? 
            offsets = iseq.offsetlines.keys
            if offsets[0] == 0 && offsets.size > 1
              "O#{offsets[1]}"
            else
              "O0"
            end
          else args.shift
          end
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
      position  = get_an_int(position_str, opts)
      condition = 'true'
      if args.size > 0 && 'if' == args[0] 
        condition_try = args[1..-1].join(' ')
        condition = condition_try if valid_condition?(condition_try)
      end
      return [position, iseq, use_offset, condition]
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
    def parse_position_one_arg(arg, old_mod=nil, show_errmsg=true)
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
      if LineCache::cached?(arg)
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

      # How about a method name with an instruction sequence?
      iseq = object_iseq(arg)
      if iseq && iseq.source_container[0] == 'file'
        filename = iseq.source_container[1]
        line_no = iseq.offsetlines.values.flatten.min
        return arg, ['file', canonic_file(filename)], line_no
      end

      if show_errmsg
        errmsg("#{arg} is not a line number, read-in filename or method " +
               "we can get location information about")
      end
      return nil, nil, nil
    end
  end
end

if __FILE__ == $0
  # Demo it.
  if  not (ARGV.size == 1 && ARGV[0] == 'noload')
    ISEQS__        = {}
    SCRIPT_ISEQS__ = {}
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    require 'thread_frame'
    require_relative %w(.. app mock)
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
    def proc.errmsg(msg)
      puts msg
    end
    puts proc.object_iseq('food').inspect
    puts proc.object_iseq('foo').inspect

    puts proc.object_iseq('foo@validate.rb').inspect
    puts proc.object_iseq('proc.object_iseq').inspect
    
    puts proc.parse_position_one_arg('tmpdir.rb').inspect
    p proc.breakpoint_position(%w(O0))
    p proc.breakpoint_position(%w(1))
    p proc.breakpoint_position(%w(2 if a > b))
  end
end
