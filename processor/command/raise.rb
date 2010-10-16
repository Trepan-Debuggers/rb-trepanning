# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::RaiseCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [exception-name]

Raise an EXCEPTION-NAME in the debugged program. If no exception name
is given, raise RuntimeError.
    HELP

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
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
    @proc.step(0)
    @proc.leave_cmd_loop = true
    @proc.core.exception = exception
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  puts cmd.run([cmd.name, 'NotanException'])
  puts cmd.run([cmd.name, '[5]'])
  puts cmd.run([cmd.name, 'RuntimeError'])
end
