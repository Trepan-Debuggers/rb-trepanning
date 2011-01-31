# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetHidelevel < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = "
#{PREFIX.join(' ')} [NUM]

Hide this many stack frames from the bottom (or least-recent) frame.

Often the bottom-most frames are setup frames that one doesn't think
of as being part of one's program but are part of the overhead loading
or debugging the program. As such generally they are unintersting in
a backtrace command and one wants these omitted.

If NUM is 0, no stack frames are hidden. If NUM is < 0 or omitted,
then use value the program sets automatically. If the value is larger
than the current number of entries in the stack, i.e. the stack shown
would be otherwise be, empty then we show all entries, or take NUM to
be 0.

Examples:
   #{PREFIX.join(' ')}     # Use the default value and hide 'uninteresting' ones
   #{PREFIX.join(' ')} 0   # Show all stack entries, even from loading the program
                     # or initial stack entries the debugger created to 
                     # debug the program.
   #{PREFIX.join(' ')} 1   # Hide only the bottom-most or least-recent stack frame.

See also 'backtrace' and 'show hidelevel'. 
."

    IN_LIST      = true
    MIN_ABBREV   = 'hide'.size
    SHORT_HELP   = "Set the number of bottom frames to hide."
  end

  def run(args)
    if args.size == 2
      val = nil
    else
      val = @proc.get_an_int(args[2])
      return unless val
    end
    @proc.hide_level = @proc.settings[:hidelevel] = val
    @proc.run_command('show hidelevel')
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this agin
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetHidelevel, false)
  cmd.run(cmd.prefix + %w(10))
  cmd.run(cmd.prefix)
end
