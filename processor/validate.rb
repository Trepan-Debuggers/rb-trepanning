# Debugger command input validation routines.
# A String is usually passed in.
class Debugger
  class CmdProcessor

    # FIXME: move to interface.
    unless defined?(YES)
      YES = %w(y yes oui si yep ja)
      NO  = %w(n no non nope nein)
      YES_OR_NO = YES + NO
    end

    # FIXME: move to interface.
    def confirm(msg, default)
      default_str = default ? 'Y/n' : 'N/y'
      while true do
        response = Readline.readline("%s (%s) " % 
                                     [msg, default_str]).strip.downcase
        if response.empty?
          response = default
          break
        end
        # We don't catch "Yes, I'm sure" or "NO!", but I leave that 
        # as an excercise for the reader.
        break if YES_OR_NO.member?(response)
        puts "Please answer 'yes' or 'no'. Try again."
      end
      return YES.member?(response)
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

    # If no argument use the default. If arg is a an integer between
    # least min_value and at_most, use that. Otherwise report an error.
    # If there's a stack frame use that in evaluation.
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
