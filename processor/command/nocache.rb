# -*- coding: utf-8 -*-
require_relative %w(base cmd)
class Debugger::Command::NoCacheCommand < Debugger::Command

  unless defined?(HELP)
    HELP         = "Remove getinlinecache instructions from instruction sequence."

    CATEGORY     = 'running'
    MAX_ARGS     = 0   # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    SHORT_HELP   = 'Remove getinlinecache instructions from instruction sequence.'

  end

  # This method runs the command
  def run(args) # :nodoc
    if @proc.frame.iseq
      puts @proc.frame.iseq.disassemble
      count = @proc.frame.iseq.killcache
      msg ("%d locations removed" % count)
      # puts @proc.frame.iseq.disassemble
    end
  end
end

if __FILE__ == $0
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
end
