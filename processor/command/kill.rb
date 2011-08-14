# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../../app/complete'
class Trepan::Command::KillCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [signal-number|signal-name]

Kill execution of program being debugged.

Equivalent of Process.kill('KILL', Process.pid). This is an unmaskable
signal. When all else fails, e.g. in thread code, use this.

If you are in interactive mode, you are prompted to confirm killing.
However when this command is aliased from a command ending in !, no 
questions are asked.

Examples:

  #{NAME}  
  #{NAME} unconditionally
  #{NAME} KILL # same as above
  #{NAME} kill # same as above
  #{NAME} -9   # same as above
  #{NAME}  9   # same as above
  #{NAME}! 9   # above, but unconditional
    HELP

    ALIASES      = %w(kill!)
    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Send this process a POSIX signal (default "9" is "kill -9")'
  end
  
  def complete(prefix)
    completions = Signal.list.keys + 
      Signal.list.values.map{|i| i.to_s} + 
      Signal.list.values.map{|i| (-i).to_s} 
    Trepan::Complete.complete_token(completions, prefix)
  end
    
  # This method runs the command
  def run(args) # :nodoc
    unconditional = ('!' == args[0][-1..-1])
    if args.size > 1
      sig = Integer(args[1]) rescue args[1]
      unless sig.is_a?(Integer) || Signal.list.member?(sig.upcase)
        errmsg("Signal name '#{sig}' is not a signal I know about.\n")
        return false
      end
    else
      if unconditional || confirm('Really quit?', false)
        sig = 'KILL'
      else 
        msg('Kill not confirmed.')
        return
      end
    end
    begin
      @proc.intf.finalize if 'KILL' == sig || Signal.list['KILL'] == sig
      Process.kill(sig, Process.pid)
    rescue Errno::ESRCH, Errno::EINVAL, Errno::EPERM, RangeError
      errmsg "Unable to send kill #{sig} to process #{Process.pid}: #{$!}"
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  %w(fooo 100 1 -1 HUP -9).each do |arg| 
    puts "#{cmd.name} #{arg}"
    cmd.run([cmd.name, arg])
    puts '=' * 40
  end
end
