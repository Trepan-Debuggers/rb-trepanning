# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../load_cmds'
class Trepan::Command::CompleteCommand < Trepan::Command

  unless defined?(HELP)
    HELP = 
"complete COMMAND-PREFIX

List the completions for the rest of the line as a command.

NOTE: For now we just handle completion of the first token.
"
    CATEGORY      = 'support'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'List the completions for the rest of the line as a command'
  end

  # This method runs the command
  def run(args) # :nodoc
    @proc.complete(args[1..-1]).each do |match|
      msg match
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  %w(d b bt).each do |prefix|
    cmd.run [cmd.name, prefix]
    puts '=' * 40
  end
  cmd.run %w(#{cmd.name} fdafsasfda)
  puts '=' * 40
end
