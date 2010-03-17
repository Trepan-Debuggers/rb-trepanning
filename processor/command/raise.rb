require_relative %w(base cmd)
class Debugger::Command::RaiseCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"raise EXCEPTION

Raise an exception in the debugged program."

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Raise an exception in the debugged program'
  end
    
  # This method runs the command
  def run(args) # :nodoc
    exception = 
      if args.size > 1
        except_str = args[1..-1].join(' ')
        # Normally would need x.respond_to? && ..
        # but since we catch errors with debug_eval.. not needed.
        eval_str = ("%s.ancestors.include?(Exception)" %
                    [except_str])
        unless @proc.debug_eval_no_errmsg(eval_str)
          errmsg "\"#{except_str}\" does not inherit Exception."
          return
        end
        @proc.debug_eval_no_errmsg(except_str)
      else
        RuntimeError
      end
    @proc.leave_cmd_loop = true
    @proc.core.exception = exception
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  puts cmd.run([name, 'NotanException'])
  puts cmd.run([name, '[5]'])
  puts cmd.run([name, 'RuntimeError'])
end
