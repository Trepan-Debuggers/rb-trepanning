# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../../app/run'
class Debugger::Command::RestartCommand < Debugger::Command

  unless defined?(HELP)
    ALIASES      = %w(R run)
    HELP = 
      'restart - Restart debugger and program via an exec
    call. All state is lost, and new copy of the debugger is used.'
    
    CATEGORY     = 'running'
    MAX_ARGS     = 0  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = '(Hard) restart of program via exec()'
  end
    
  # This method runs the command
  def run(args) # :nodoc

    dbgr = @proc.core.dbgr
    argv = dbgr.restart_argv
    if argv and argv.size > 0
      unless File.executable?(argv[0])
        msg(["File #{argv[0]} not executable.",
             "Adding Ruby interpreter."])
        argv.unshift Rbdbgr::ruby_path
      end
      @proc.run_cmd(%w(show args))
      if not confirm('Restart (exec)?', false)
        msg "Restart not confirmed"
      else
        msg 'Restarting...'
        @proc.run_cmd(%w(save))
        argv.unshift
        # FIXME: Run atexit finalize routines?
        Dir.chdir(dbgr.initial_dir) if dbgr.initial_dir
        exec(*argv)
      end
    else
      errmsg("No executable file and command options recorded.")
    end
  end
end

if __FILE__ == $0
  exit if ARGV[0] == 'exit'

  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  dbgr.restart_argv = []
  cmd.run([name])
  dbgr.restart_argv = [File.expand_path($0), 'exit']
  cmd.run([name])
end
