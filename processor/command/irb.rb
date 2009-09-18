require 'irb'
require_relative 'base_cmd'
require_relative %w(.. .. lib irb)
class Debugger::Command::IRBCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"          irb [-d]\tstarts an Interactive Ruby (IRB) session.

If -d is added you can get access to debugger frame the global variable
$rbdbgr_frame. 

irb is extended with methods 'cont', 'n' and 'step' which 
run the corresponding debugger commands. In contrast to the real debugger
commands these commands don't allow command arguments.
"

    CATEGORY     = 'support'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Run interactive Ruby session irb as a command subshell'
  end
    
  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      add_debugging = '-d' == args[1]
      # FIXME -d ? 
    end

    # unless @state.interface.kind_of?(LocalInterface)
    #   print "Command is available only in local mode.\n"
    #   throw :debug_error
    # end

    save_trap = trap("SIGINT") do
      throw :IRB_EXIT, :cont if $rbdbgr_in_irb
    end

    $rbdbgr_frame = @proc.frame if add_debugging
    $rbdbgr_in_irb = true
    cont = IRB.start_session(@proc.frame.binding)
    case cont
    when :cont
      @proc.continue
    when :step
      @proc.step(1, {})
    # when :next
    #   force = Command.settings[:force_stepping]
    #   @state.context.step_over(1, @state.frame_pos, force)
    #   @state.proceed 
    else
      @proc.print_location
    end
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  MockDebugger::show_special_class_constants(cmd)
end
