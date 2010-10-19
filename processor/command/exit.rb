# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::ExitCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [exitcode] 

hard exit of the debugged program.  

The program being debugged is exited via exit!() which does not run
the Kernel at_exit finalizers. If a return code is given, that is the
return code passed to exit() - presumably the return code that will be
passed back to the OS. If no exit code is given, 0 is used.

See also the commands "quit" and "kill".
    HELP

    CATEGORY     = 'support'
    MAX_ARGS     = 2  # Need at most this many
    SHORT_HELP  = 'Exit program via "exit!"'
  end

  # FIXME: Combine 'quit' and 'exit'. The only difference is
  # whether exit! or exit is used.

  # This method runs the command
  def run(args)
    unconditional = 
      if args.size > 1 && args[1] == 'unconditionally'
        args.shift
        true
      elsif args[0][-1..-1] == '!'
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
  dbgr, cmd = MockDebugger::setup
  fork { cmd.run([cmd.name]) }
  cmd.run([cmd.name, '10'])
end
