# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ReloadCommand < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-HELP
reload command [file-or-directory ...]

Loads or reloads debugger commands. If no parameters are passed all 
commands are reloaded. If a parameters are passed each is expected to 
be a either a Ruby file which implements a series of debugger
commands by defining Trepan::Command classes, or a directory which contains
files that implements debugger commands.
    HELP
    MIN_ABBREV   = 'co'.size # Note we have "info file"
    NEED_STACK   = true
    SHORT_HELP   = 'Reload debugger commmands from debugger directories'
  end

  def run(args)
    if args.size == 2
      @proc.load_cmds_initialize
      msg('Debugger commands reloaded.')
    elsif args.size > 2
      args[2..-1].each do |name|
        if @proc.load_debugger_commands(name)
          msg "Debugger command file #{name} loaded."
        else
          errmsg "Can't load file or directory #{name}"
        end
      end
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::ReloadCommand, false)
  cmd.run(cmd.prefix)
  dir = File.dirname(__FILE__)
  cmd.run(cmd.prefix + [File.join(%W(#{dir} .. list.rb))])
  cmd.run(cmd.prefix + [File.join(%W(#{dir} nonexistent-command.rb))])
end
