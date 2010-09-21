# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::ExitCommand < Trepan::Command

  unless defined?(HELP)
    HELP = 
      'exit [exitcode] - hard exit of the debugged program.  

The program being debugged is exited via exit!() which does not run
the Kernel at_exit finalizers. If a return code is given, that is the
return code passed to exit() - presumably the return code that will be
passed back to the OS. If no exit code is given, 0 is used.

See also the commands "quit" and "kill".'

    CATEGORY     = 'support'
    MAX_ARGS     = 2  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Exit program via "exit!"'
  end

  # FIXME: Combine 'quit' and 'exit'. The only difference is
  # whether exit! or exit is used.

  # This method runs the command
  def run(args) # :nodoc
    unconditional = 
      if args.size > 1 && args[1] == 'unconditionally'
        args.shift
        true
      else
        false
      end
    unless unconditional || confirm('Really quit?', false)
      msg('Quit not confirmed.')
      return
    end
    exitrc = (args.size > 1) ? exitrc = Integer(args[1]) rescue 0 : 0
    # No graceful way to stop threads...
    # A little harsh, but for now let's go with this.
    exit! exitrc
  end
end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  name = File.basename(__FILE__, '.rb')
  fork { cmd.run([name]) }
  cmd.run([name, '10'])
end
