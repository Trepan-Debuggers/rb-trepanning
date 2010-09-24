# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'up'

# Debugger "down" command. Is the same as the "up" command with the 
# direction (set by DIRECTION) reversed.
class Trepan::Command::DownCommand < Trepan::Command::UpCommand

  # Silence already initialized constant .. warnings
  old_verbose = $VERBOSE  
  $VERBOSE    = nil
  HELP = 
"d(own) [count]

Move the current frame down in the stack trace (to a newer frame). 0
is the most recent frame. If no count is given, move down 1.

See also 'up' and 'frame'.
"

  ALIASES       = %w(d)
  NAME          = File.basename(__FILE__, '.rb')
  SHORT_HELP    = 'Move frame in the direction of the caller of the last-selected frame'
  $VERBOSE      = old_verbose 

  def initialize(proc)
    super
    @direction = -1 # +1 for up.
  end

end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  def sep ; puts '=' * 40 end
  cmd.run [name]
  %w(-1 0 1 -2).each do |count| 
    puts "#{name} #{count}"
    cmd.run([name, count])
    sep 
  end
  def foo(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    puts "#{name}"
    cmd.run([name])
    sep
    puts "#{name} -1"
    cmd.run([name, '-1'])
  end
  foo(cmd, name)
end
