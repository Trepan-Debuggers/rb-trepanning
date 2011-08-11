# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../load_cmds'
class Trepan::Command::CompleteCommand < Trepan::Command

  unless defined?(HELP)
    NAME          = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} COMMAND-PREFIX

List the completions for the rest of the line as a command.
    HELP
    CATEGORY      = 'support'
    NEED_STACK    = false
    SHORT_HELP    = 'List the completions for the rest of the line as a command'
  end

  # This method runs the command
  def run(args) # :nodoc
    last_arg = @proc.cmd_argstr.end_with?(' ') ? '' : args[-1]
    @proc.complete(@proc.cmd_argstr, last_arg).each do |match|
      msg match
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  %w(d b bt).each do |prefix|
    cmd.proc.instance_variable_set('@cmd_argstr', prefix)
    cmd.run [cmd.name, prefix]
    puts '=' * 40
  end
  cmd.run %w(#{cmd.name} fdafsasfda)
  puts '=' * 40
end
