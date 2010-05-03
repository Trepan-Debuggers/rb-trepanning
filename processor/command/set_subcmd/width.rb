# -*- coding: utf-8 -*-
require_relative '../base/subcmd'

class Debugger::Subcommand::SetWidth < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Set number of characters the debugger thinks are in a line'
    IN_LIST      = true
    MIN_ABBREV   = 'wid'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set width)
  end

  def run(args)
    if args.size >= 3 
      run_set_int(args[2..-1].join(' '),
                  "The 'width' command requires a line width", 
                  0, nil)
    else
      errmsg "Too few arguments - the 'width' command requires a line width"
    end
  end

  alias restore_command restore_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  dbgr, cmd = MockDebugger::setup('set')
  subcommand = Debugger::Subcommand::SetWidth.new(cmd)
  subcommand.run(%w(20))
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  puts
  puts '-' * 20
  puts subcommand.restore_command()
end
