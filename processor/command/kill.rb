require_relative 'base_cmd'
class Debugger::Command::KillCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"Kill execution of program being debugged.

Equivalent of Process.kill( -KILL <pid> where <pid> is os.getpid(), the current
debugged process. This is an unmaskable signal. When all else fails, e.g. in
thread code, use this.

If 'unconditionally' is given, no questions are asked. Otherwise, if
we are in interactive mode, we'll prompt to make sure."

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Send this process a POSIX signal ("9" for "kill -9")'
  end
    
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
    begin
      Process.kill(sig, Process.pid)
    rescue Errno::ESRCH
      errmsg "Unable to send kill #{sig} to process #{Process.pid}"
    end
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  %w(fooo 1 -1 HUP -9).each do |arg| 
    puts "#{name} #{arg}"
    cmd.run([name, arg])
    puts '=' * 40
  end
end
