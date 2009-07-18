require_relative 'base_cmd'
class Debugger::ExitCommand < Debugger::Command

  HELP = 
'exit [exitcode] - hard exit of the debugged program.  
The program being debugged is exited via exit(). If a return code
is given that is the return code passed to exit() - presumably the
return code that will be passed back to the OS.'

  CATEGORY     = 'support'
  MIN_ARGS     = 0  # Need at least this many
  MAX_ARGS     = 1  # Need at most this many

  # First entry is the name of the command. Any aliases for the
  # command follow.
  NAME_ALIASES = %w(exit)

  SHORT_HELP  = 'Exit program via exit()'

  # This method runs the command
  def run(args) # :nodoc
    # A little harsh, but for now let's go with this.
    p 'calling it quits'
    exitrc = Integer(args[1]) rescue 0 if args.size > 1
    exit exitrc
  end
end

if __FILE__ == $0
  cmd = Debugger::ExitCommand.new
  p cmd.class.const_get(:NAME_ALIASES)
  cmd.run %w(exit 10)
end
