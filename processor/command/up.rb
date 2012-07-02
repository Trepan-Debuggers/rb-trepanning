# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../../app/util'

# up command. Like 'down' but the direction (set by DIRECTION) is different.
#
# NOTE: The down command  subclasses this, so beware when changing! 
class Trepan::Command::UpCommand < Trepan::Command

  Trepan::Util.suppress_warnings {
    NAME        = File.basename(__FILE__, '.rb')
    HELP        = <<-HELP
#{NAME} [count]

Move the current frame up in the stack trace (to an older frame). 0 is
the most recent frame. If no count is given, move up 1.

See also 'down' and 'frame'.
  HELP

    ALIASES       = %w(u)
    CATEGORY      = 'stack'
    MAX_ARGS      = 1  # Need at most this many
    NEED_STACK    = true
    SHORT_HELP    = 'Move frame in the direction of the caller of the last-selected frame'
  }
  
  require_relative '../../app/frame'
  include Trepan::Frame

  def complete(prefix)
    @proc.frame_complete(prefix, @direction)
  end
  
  def initialize(proc)
    super
    @direction = +1 # -1 for down.
  end

  # Run 'up' command. 
  def run(args)

    # FIXME: move into @proc and test based on NEED_STACK.
    if not @proc.top_frame
      errmsg('No frames recorded.')
      return false
    end

    if args.size == 1
      # Form is: "down" which means "down 1"
      count = 1
    else
      low, high = @proc.frame_low_high(@direction)
      count_str = args[1]
      name_or_id = args[1]
      opts = {
        :msg_on_error => 
        "The '#{NAME}' command argument requires a frame number. Got: %s" % count_str,
        :min_value => low, :max_value => high
      }
      count = @proc.get_an_int(count_str, opts)
      return false unless count
    end
    @proc.adjust_frame(@direction * count, false)
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup

  def sep ; puts '=' * 40 end
  cmd.run [cmd.name]
  %w(-1 0 1 -2).each do |count| 
    puts "#{cmd.name} #{count}"
    cmd.run([cmd.name, count])
    sep 
  end
  def foo(cmd, name)
    cmd.proc.frame_setup(RubyVM::Frame::current)
    puts "#{name}"
    cmd.run([name])
    sep
    %w(-2 -1).each do |count| 
      puts "#{name} #{count}"
      cmd.run([name, count])
      sep 
    end
  end
  foo(cmd, cmd.name)
end
