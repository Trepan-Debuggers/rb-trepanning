# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::KillCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [signal-number|signal-name|unconditionally]

Kill execution of program being debugged.

Equivalent of Process.kill('KILL', Process.pid). This is an unmaskable
signal. When all else fails, e.g. in thread code, use this.

If 'unconditionally' is given, no questions are asked. Otherwise, if
we are in interactive mode, we'll prompt to make sure.

Examples:

  #{NAME}  
  #{NAME} unconditionally
  #{NAME} KILL # same as above
  #{NAME} kill # same as above
  #{NAME} -9   # same as above
  #{NAME} 9    # same as above
    HELP

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Send this process a POSIX signal (default "9" is "kill -9")'
  end
    
  # This method runs the command
  def run(args)
    if args.size > 1
      sig = Integer(args[1]) rescue args[1]
      unless sig.is_a?(Integer) || Signal.list.member?(sig.upcase)
        errmsg("Signal name '#{sig}' is not a signal I know about.\n")
        return false
        end
#       FIXME: reinstate
#       if 'KILL' == sig || Signal['KILL'] == sig
#           @proc.intf.finalize
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
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  %w(fooo 1 -1 HUP -9).each do |arg| 
    puts "#{cmd.name} #{arg}"
    cmd.run([cmd.name, arg])
    puts '=' * 40
  end
end
