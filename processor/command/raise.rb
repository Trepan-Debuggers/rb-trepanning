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
    except = 
      if args.size > 1
        except_str = args[1]
        unless @proc.debug_eval_no_errmsg("#{except_str}.is_a?(Class)")
          errmsg "#{except_str} is not known to be a Class\n"
          return
        end
        @proc.debug_eval_no_errmsg(except_str)
      else
        RuntimeError
      end
    @proc.leave_cmd_loop = true
    @proc.core.exception = except
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  puts cmd.run([name, 'RuntimeError'])
end
