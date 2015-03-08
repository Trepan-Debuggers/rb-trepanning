# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../../app/run'
class Trepan::Command::RestartCommand < Trepan::Command

  unless defined?(HELP)
    NAME         = File.basename(__FILE__, '.rb')
    ALIASES      = %w(R run)
    HELP = <<-HELP
**#{NAME}**

Restart debugger and program via an exec call. All state is lost, and
new copy of the debugger is used.

See also:
---------

`set args`, `show args`
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
      @proc.run_cmd(%w(show args))
      if not confirm('Restart (exec)?', false)
        msg "Restart not confirmed"
      else
        if defined?(Trepan::PROG_UNRESOLVED_SCRIPT) &&
            position = argv.index(Trepan::PROG_UNRESOLVED_SCRIPT)
          save_filename = @proc.save_commands(:erase =>true)
          argv.insert(position, '--command', save_filename) if save_filename
        end
        Dir.chdir(RubyVM::OS_STARTUP_DIR)
        msg 'Restarting using...'
        msg "\t #{argv.inspect}"
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
