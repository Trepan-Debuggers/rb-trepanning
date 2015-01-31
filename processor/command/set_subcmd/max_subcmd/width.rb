# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetMaxWidth < Trepan::SubSubcommand
    unless defined?(HELP)
        HELP         = 'Set max[imum] width NUMBER

Set number of characters the debugger thinks are in a line.'
        IN_LIST      = true
        MIN_ABBREV   = 'wid'.size
        NAME         = File.basename(__FILE__, '.rb')
        PREFIX       = %w(set max width)
        SHORT_HELP   = 'Set number of characters the debugger thinks are in a line'
    end

    def display_width
        (ENV['COLUMNS'] || `tput cols &2>/dev/null`).to_i rescue 80
    end

    def run(args)
        args.shift
        if args.size == 0
            @proc.settings[:maxwidth] = display_width
            run_show_int
        else
            run_set_int(args.join(' '),
                        "The '#{PREFIX.join(' ')}' command requires a line width",
                        0, nil)
        end
    end

    alias save_command save_command_from_settings

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  name = File.basename(__FILE__, '.rb')

  dbgr, set_cmd = MockDebugger::setup('set')
  max_cmd       = Trepan::SubSubcommand::SetMax.new(dbgr.core.processor,
                                                      set_cmd)
  cmd_ary       = Trepan::SubSubcommand::SetMaxWidth::PREFIX
  cmd_name      = cmd_ary.join(' ')
  subcmd        = Trepan::SubSubcommand::SetMaxWidth.new(set_cmd.proc,
                                                         max_cmd,
                                                         cmd_name)
  prefix_run = cmd_ary[1..-1]
  subcmd.run(prefix_run)
  subcmd.run(prefix_run + %w(0))
  subcmd.run(prefix_run + %w(20))
  name = File.basename(__FILE__, '.rb')
  subcmd.summary_help(name)
  puts
  puts '-' * 20
  puts subcmd.save_command
end
