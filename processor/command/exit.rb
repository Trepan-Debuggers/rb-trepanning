require_relative %w(base cmd)
class Debugger::Command::ExitCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
      'exit [exitcode] - hard exit of the debugged program.  
The program being debugged is exited via exit(). If a return code
is given that is the return code passed to exit() - presumably the
return code that will be passed back to the OS.'
    
    CATEGORY     = 'support'
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Exit program via "exit!"'
  end

  # This method runs the command
  def run(args) # :nodoc
    # A little harsh, but for now let's go with this.
    msg 'calling it quits'
    exitrc = (args.size > 1) ? exitrc = Integer(args[1]) rescue 0 : 0
    # No graceful way to stop threads...
    exit! exitrc
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  name = File.basename(__FILE__, '.rb')
  cmd.run([name, '10'])
end
