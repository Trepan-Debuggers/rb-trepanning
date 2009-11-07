# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. .. lib run)
class Debugger::Command::RestartCommand < Debugger::Command

  unless defined?(HELP)
    ALIASES      = %w(R)
    HELP = 
      'restart - Restart debugger and program via an exec
    call. All state is lost, and new copy of the debugger is used.'
    
    CATEGORY     = 'support'
    MAX_ARGS     = 0  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = '(Hard) restart of program via exec()'
  end
    
  # This method runs the command
  def run(args) # :nodoc

    argv = @proc.core.dbgr.restart_argv
    if argv and argv.size > 0
      unless File.executable?(argv[0])
        msg(["File #{argv[0]} not executable.",
             "Adding Ruby interpreter."])
        argv.unshift Rbdbgr::ruby_path
      end
      msg("Restart args:\n\t#{argv.inspect}")
      if not confirm('Restart (exec)?', false)
        msg "Restart not confirmed"
      else
        msg 'Restarting...'
        # FIXME: Run atexit finalize routines?
        exec(*argv)
      end
    else
      errmsg("No executable file and command options recorded.")
    end
  end
end

if __FILE__ == $0
  exit if ARGV[0] == 'exit'

  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  dbgr.restart_argv = []
  cmd.run([name])
  dbgr.restart_argv = [File.expand_path($0), 'exit']
  cmd.run([name])
end
