# -*- coding: utf-8 -*-
# Copyright (C) 2010-2012 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::Subcommand::SetAutoList < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP = "Set to run a 'list' command each time we enter the debugger"
    MIN_ABBREV = 'l'.size
    SHORT_HELP = "Set running a 'list' command each time we enter the debugger"
  end

  def run(args)
    super
    if @proc.settings[:autolist]
      @proc.cmdloop_prehooks.insert_if_new(10, *@proc.autolist_hook)
    else
      @proc.cmdloop_prehooks.delete_by_name('autolist')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../auto'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetAuto,
                                   Trepan::SubSubcommand::SetAutoList)
  %w(off on 0 1).each { |arg| cmd.run([cmd.name, arg]) }
  puts '-' * 10
  puts cmd.save_command.join("\n")
end
