require_relative 'base_cmd'
class Debugger::Command::KillCommand < Debugger::Command

  HELP = 
"Kill execution of program being debugged.

Equivalent of Process.kill( -KILL <pid> where <pid> is os.getpid(), the current
debugged process. This is an unmaskable signal. When all else fails, e.g. in
thread code, use this.

If 'unconditionally' is given, no questions are asked. Otherwise, if
we are in interactive mode, we'll prompt to make sure."

  CATEGORY     = 'running'
  MIN_ARGS     = 0  # Need at least this many
  MAX_ARGS     = 1  # Need at most this many
  NAME         = File.basename(__FILE__, '.rb')
  SHORT_HELP  = 'Send this process a POSIX signal ("9" for "kill -9")'

  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      sig = Integer(args[1]) rescue args[1]
      unless sig.is_a?(Integer) || Signal.list.member?(sig)
        errmsg("Signal name '#{sig}' is not a signal I know about.\n")
        return false
        end
#       FIXME: reinstate
#       if 'KILL' == sig || Signal['KILL'] == sig
#           @state.interface.finalize
#       end
    else
      if not confirm('Really kill?', false)
        msg('Kill not confirmed.')
        return
      else 
        sig = 'KILL'
      end
    end
    Process.kill(sig, Process.pid)
  end
end

if __FILE__ == $0
  name = File.basename(__FILE__, '.rb')
  cmd = Debugger::Command::KillCommand.new(nil)
  def cmd.errmsg(msg)
    puts msg
  end
  cmd.run([name, 'fooo'])
  cmd.run([name, '-9'])
end
