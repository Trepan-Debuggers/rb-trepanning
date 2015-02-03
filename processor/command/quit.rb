# Copyright (C) 2010-2011, 2013 Rocky Bernstein <rockyb@rubyforge.net>
require 'rbconfig'
require_relative '../command'
class Trepan::Command::QuitCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
**#{NAME}**[!] [**unconditionally**] [*exit-code*]

Gentle termination.

The program being debugged is exited via *exit()* which runs the Kernel
*at_exit* finalizers. If a return code is given, that is the return code
passed to *exit()*; presumably the return code that will be passed back
to the OS. If no exit code is given, 0 is used.

Examples:
---------
    #{NAME}                 # quit prompting if we are interactive
    #{NAME} unconditionally # quit without prompting
    #{NAME}!                # same as above
    #{NAME} 0               # same as "quit"
    #{NAME}! 1              # unconditional quit setting exit code 1

See also:
---------
`exit`, `kill`
    HELP

    ALIASES      = %W(#{NAME}! q q!)
    CATEGORY     = 'support'
    MAX_ARGS     = 2  # Need at most this many
    SHORT_HELP  = 'Quit program - gently'
  end

  # FIXME: Combine 'quit' and 'exit'. The only difference is
  # whether exit! or exit is used.

  # This method runs the command
  def run(args)
    if RbConfig::CONFIG['target_os'].start_with?('mingw')
        return @proc.commands['exit'].run(['exit', args])
    end
    unconditional =
      if args.size > 1 && args[-1] == 'unconditionally'
        args.shift
        true
      elsif args[0][-1] == '!'
        true
      else
        false
      end
    unless unconditional || confirm('Really quit?', false)
      msg('Quit not confirmed.')
      return
    end

    if (args.size > 1)
      if  args[1] =~ /\d+/
        exitrc = args[1].to_i;
      else
        errmsg "Bad an Integer return type \"#{args[1]}\"";
        return;
      end
    else
      exitrc = 0
    end
    # No graceful way to stop threads...
    @proc.finalize
    @proc.dbgr.intf[-1].finalize
    exit exitrc
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  Process.fork { cmd.run([cmd.name]) } if
    Process.respond_to?(:fork)
  cmd.run([cmd.name, 'foo'])
  cmd.run([cmd.name, '5'])
end
