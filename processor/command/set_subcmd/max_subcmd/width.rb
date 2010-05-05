# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::SetMaxWidth < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Set max[imum] width NUMBER

Set number of characters the debugger thinks are in a line.'
    IN_LIST      = true
    MIN_ABBREV   = 'wid'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set max width)
    SHORT_HELP   = 'Set number of characters the debugger thinks are in a line'
  end

  def run(args)
    args.shift
    if args.size >= 3 
      run_set_int(args[2..-1].join(' '),
                  "The 'width' command requires a line width", 
                  0, nil)
    else
      errmsg "Too few arguments - the 'width' command requires a line width"
    end
  end

  alias save_command save_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  name = File.basename(__FILE__, '.rb')

  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Debugger::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                      set_cmd)
  cmd_name      = Debugger::SubSubcommand::SetMaxWidth::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetMaxWidth.new(set_cmd.proc,
                                                           max_cmd,
                                                           cmd_name)
  subcmd.run(['max', 'width'])
  subcmd.run(%w(set max width 0))
  subcmd.run(%w(set max width 20))
  name = File.basename(__FILE__, '.rb')
  subcmd.summary_help(name)
  puts
  puts '-' * 20
  puts subcmd.save_command
end
