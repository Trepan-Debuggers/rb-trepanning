require_relative 'base_cmd'
class Debugger::ExitCommand < Debugger::Command

  # First entry is the name of the command. Any aliases for the
  # command follow.
  @help = 
'exit [exitcode] - hard exit of the debugged program.  
The program being debugged is exited via sys.exit(). If a return code
is given that is the return code passed to sys.exit() - presumably the
return code that will be passed back to the OS.'

  @name_aliases = %w(exit)
  @short_help   = 'Exit program via exit()'

  # This method runs the command
  def run(args) :nodoc
    # A little harsh, but for now let's go with this.
    p 'calling it quits'
    exitrc = Integer(args[1]) rescue 0 if args.size > 1
    exit exitrc
  end
end

p "quit loaded" if $DEBUG
if __FILE__ == $0
  cmd = Debugger::ExitCommand.new
  p cmd.class.name_aliases
  cmd.run %w(exit 10)
end
