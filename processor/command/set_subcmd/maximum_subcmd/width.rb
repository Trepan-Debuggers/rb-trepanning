# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'

class Debugger::SubSubcommand::SetMaximumWidth < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 'Set max[imum] width NUMBER

Set number of characters the debugger thinks are in a line.'
    IN_LIST      = true
    MIN_ABBREV   = 'wid'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set maximum width)
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

  alias restore_command restore_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  name = File.basename(__FILE__, '.rb')

  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Debugger::SubSubcommand::SetMaximum.new(dbgr.core.processor, 
                                                          set_cmd)
  cmd_name      = Debugger::SubSubcommand::SetMaximumWidth::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetMaximumWidth.new(set_cmd.proc,
                                                               max_cmd,
                                                               cmd_name)
  subcmd.run(['maximum', 'width'])
  subcmd.run(%w(max width 0))
  subcmd.run(%w(max width 20))
  name = File.basename(__FILE__, '.rb')
  subcmd.summary_help(name)
  puts
  puts '-' * 20
  puts subcmd.restore_command()
end
