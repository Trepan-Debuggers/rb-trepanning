require_relative 'base_cmd'
class Debugger::Command::ExitCommand < Debugger::Command

  HELP = 
'exit [exitcode] - hard exit of the debugged program.  
The program being debugged is exited via exit(). If a return code
is given that is the return code passed to exit() - presumably the
return code that will be passed back to the OS.'

  CATEGORY     = 'support'
  MIN_ARGS     = 0  # Need at least this many
  MAX_ARGS     = 1  # Need at most this many
  NAME         = File.basename(__FILE__, '.rb')
<<<<<<< HEAD:processor/command/exit.rb

=======
>>>>>>> cdcfba71a1e69d3158faa475f4f616287bb2afb3:processor/command/exit.rb
  SHORT_HELP  = 'Exit program via exit()'

  # This method runs the command
  def run(args) # :nodoc
    # A little harsh, but for now let's go with this.
    p 'calling it quits'
    exitrc = (args.size > 1) ? exitrc = Integer(args[1]) rescue 0 : 0
  end
end

if __FILE__ == $0
  cmd = Debugger::Command::ExitCommand.new(nil)
<<<<<<< HEAD:processor/command/exit.rb
=======
  p cmd.class.const_get(:NAME)
>>>>>>> cdcfba71a1e69d3158faa475f4f616287bb2afb3:processor/command/exit.rb
  cmd.run %w(exit 10)
end
