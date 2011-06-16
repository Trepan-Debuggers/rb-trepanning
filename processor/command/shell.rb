# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'irb'
require_relative 'base/cmd'
class Trepan::Command::IRBCommand < Trepan::Command

  unless defined?(HELP)
    require_relative '../../app/irb'
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [-d]

Start an Interactive Ruby (IRB) session.

If -d is added you can get access to debugger frame the global variables
$trepan_frame and $trepan_cmdproc. 

#{NAME} is extended with methods 'cont', 'ne', and, 'q', 'step' which 
run the corresponding debugger commands 'continue', 'next', 'exit' and 'step'. 

To issue a debugger command, inside #{NAME} nested inside a debugger use
'dbgr'. For example:

  dbgr %w(info program)
  dbgr('info', 'program') # Same as above
  dbgr 'info program'     # Single quoted string also works

But arguments have to be quoted because #{NAME} will evaluate them:

  dbgr info program     # wrong!
  dbgr info, program    # wrong!
  dbgr(info, program)   # What I say 3 times is wrong!

Here then is a loop to query VM stack values:
  (-1..1).each {|i| dbgr(\"info reg sp \#{i}\")}
     HELP

    ALIASES    = %w(irb)
    CATEGORY   = 'support'
    MAX_ARGS   = 1  # Need at most this many
    SHORT_HELP = "Run #{NAME} as a command subshell"
  end

  # This method runs the command
  def run(args)
    add_debugging = 
      if args.size > 1
        '-d' == args[1]
      else
        false
      end

    # unless @state.interface.kind_of?(LocalInterface)
    #   print "Command is available only in local mode.\n"
    #   throw :debug_error
    # end

    save_trap = trap('SIGINT') do
      throw :IRB_EXIT, :cont if $trepan_in_irb
    end

    $trepan = @proc.dbgr 
    if add_debugging
      $trepan_cmdproc  = @proc
      $trepan_frame    = @proc.frame
    end
    $trepan_in_irb = true
    $trepan_irb_statements = nil
    $trepan_command = nil

    conf = {:BACK_TRACE_LIMIT => settings[:maxstack],
            :RC => true}

    # ?? Should we set GNU readline to what we have,
    # or let IRB make its own determination? 

    # Save the Readline history and set the Readline completion function 
    # to be IRB's function 
    if Trepan::GNU_readline? 
      @proc.intf.save_history if @proc.intf.respond_to?(:save_history)
      require 'irb/completion'
      Readline.completion_proc = IRB::InputCompletor::CompletionProc
    end

    # And just when you thought, we'd never get around to 
    # actually running irb...
    cont = IRB.start_session(@proc.frame.binding, @proc, conf)
    trap('SIGINT', save_trap) # Restore our old interrupt function.

    # Restore the debuggers' Readline history and the Readline completion 
    # function
    if Trepan::GNU_readline? && @proc.dbgr.completion_proc
      @proc.intf.read_history if @proc.intf.respond_to?(:read_history)
      Readline.completion_proc = @proc.dbgr.completion_proc 
    end

    # Respect any backtrace limit set in irb.
    back_trace_limit = IRB.CurrentContext.back_trace_limit
    if settings[:maxstack] !=  back_trace_limit
      msg("\nSetting debugger's BACK_TRACE_LIMIT (%d) to match irb's last setting (%d)" % 
          [settings[:maxstack], back_trace_limit])
      settings[:maxstack]= IRB.CurrentContext.back_trace_limit
    end

    case cont
    when :continue
      @proc.continue
    when :finish
      @proc.finish 
    when :next
      @proc.next # (1, {})
    when :quit
      @proc.quit
    when :step
      @proc.step # (1, {})
    else
      @proc.print_location
    end
  ensure
    $trepan_in_irb = false
    # restore old trap if any
    trap('SIGINT', save_trap) if save_trap
  end
end

if __FILE__ == $0
  require 'thread_frame'
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  # Get an IRB session -- the hard way :-)
  cmd.run([cmd.name]) if ARGV.size > 0
end
