require_relative 'base_cmd'
require_relative File.join(%w(.. .. lib frame))
class Debugger::WhereCommand < Debugger::Command

  include Debugger::Frame

  unless defined?(HELP)
    HELP = 
"where [count]

Print a stack trace, with the most recent frame at the top.  With a
positive number, print at most many entries.  With a negative number
print the top entries minus that number.

An arrow indicates the 'current frame'. The current frame determines
the context used for many debugger commands such as expression
evaluation or source-line listing.

Examples:
   where    # Print a full stack trace
   where 2  # Print only the top two entries
   where -1 # Print a stack trace except the initial (least recent) call."

    CATEGORY     = 'stack'
    MIN_ARGS     = 0  # Need at least this many
    MAX_ARGS     = 1  # Need at most this many
    
    # First entry is the name of the command. Any aliases for the
    # command follow.
    NAME_ALIASES = %w(where bt backtrace)
    NEED_STACK   = true
    
    SHORT_HELP  = 'Print backtrace of stack frames'
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      # Deal with this better later.
      count = Integer(args[1]) rescue nil
    end
    if @core and @core.frame
      print_stack_trace(@core.frame, count)
    elsif @proc
      @proc.errmsg 'No frame'
    end
    return false  # Don't break out of cmd loop
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative File.join(%w(.. .. lib core))
  require 'thread_frame'
  core = Debugger::Core.new()
  cmd = Debugger::WhereCommand.new
  cmd.core = core
  core.frame = RubyVM::ThreadFrame::current
  p cmd.class.const_get(:NAME_ALIASES)
  cmd.run %w(where)
  puts '=' * 40
  cmd.run %w(where 1)
  puts '=' * 40
  def foo(core, cmd)
    core.frame = RubyVM::ThreadFrame::current
    cmd.run(%w(where))
  end
  foo(core, cmd)
end
