# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetMaxList < Trepan::SubSubcommand
  unless defined?(HELP)
    NAME         = File.basename(__FILE__, '.rb')
    IN_LIST      = true
    MIN_ABBREV   = 'lis'.size
    PREFIX       = %W(set max #{NAME})
    SHORT_HELP   = 'Set number of lines to list'
    HELP         = <<-EOH
**#{PREFIX.join(' ')}** *integer-expression*

Set number of source-code lines to list by default in a debugger `list` command.

See also:
---------
`list`, `show maximum list`
EOH

  end

  def run(args)
    args.shift
    run_set_int(args.join(' '),
                "The '#{PREFIX.join(' ')}' command requires a list size",
                0, nil)
  end

  alias save_command save_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../max'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetMax,
                                   Trepan::SubSubcommand::SetMaxList, false)
  prefix_run = cmd.prefix[1..-1]
  cmd.run(prefix_run)
  cmd.run(prefix_run + %w(0))
  cmd.run(prefix_run + %w(20))
  puts cmd.save_command
end
