# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../../app/run'
class Trepan::Command::RestartCommand < Trepan::Command

  unless defined?(HELP)
    NAME         = File.basename(__FILE__, '.rb')
    ALIASES      = %w(R run)
    HELP = <<-HELP
#{NAME} 

Restart debugger and program via an exec call. All state is lost, and
new copy of the debugger is used.
    HELP
    
    CATEGORY     = 'running'
    MAX_ARGS     = 0  # Need at most this many
    SHORT_HELP  = '(Hard) restart of program via exec()'
  end
    
  # This method runs the command
  def run(args)

    dbgr = @proc.dbgr
    argv = dbgr.restart_argv
    if argv and argv.size > 0
      # unless File.executable?(argv[0])
      #   msg(["File #{argv[0]} not executable.",
      #        "Adding Ruby interpreter."])
      #   argv.unshift Trepanning::ruby_path
      # end
      @proc.run_cmd(%w(show args))
      if not confirm('Restart (exec)?', false)
        msg "Restart not confirmed"
      else
        msg 'Restarting...'
        @proc.run_cmd(%w(save))
        Dir.chdir(RubyVM::OS_STARTUP_DIR)
        @proc.finalize
        exec(*argv)
      end
    else
      errmsg("No executable file and command options recorded.")
    end
  end
end

if __FILE__ == $0
  exit if ARGV[-1] == 'exit'
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  dbgr.restart_argv = []
  cmd.run([cmd.name])
  dbgr.restart_argv = RubyVM::OS_ARGV + ['exit']
  cmd.run([cmd.name])
end
