# Copyright (C) 2010-2011, 2013 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'

class Trepan::Command::AliasCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} ALIAS COMMAND

Add alias ALIAS for a debugger command COMMAND.  

Add an alias when you want to use a command abbreviation for a command
that would otherwise be ambigous. For example, by default we make 's'
be an alias of 'step' to force it to be used. Without the alias, "s"
might be "step", "show", or "set" among others

Example:

alias cat list   # "cat rubyfile.rb" is the same as "list rubyfile.rb"
alias s   step   # "s" is now an alias for "step".
                 # The above example is done by default.

See also 'unalias' and 'show #{NAME}'.
    HELP

    CATEGORY      = 'support'
    MAX_ARGS      = 2  # Need at most this many
    NEED_STACK    = true
    SHORT_HELP    = 'Add an alias for a debugger command'
  end
  
  # Run command. 
  def run(args)
    if args.size == 1
      @proc.commands['show'].run(%W(show #{NAME}))
    elsif args.size == 2
      @proc.commands['show'].run(%W(show #{NAME} #{args[1]}))
    else
      junk, al, command = args
      old_command = @proc.aliases[al]
      if @proc.commands.member?(command)
        @proc.aliases[al] = command
        if old_command
          msg("Alias '#{al}' for command '#{command}' replaced old " + 
              "alias for '#{old_command}'.")
        else
          msg "New alias '#{al}' for command '#{command}' created."
        end
      else
        errmsg "You must alias to a command name, and '#{command}' isn't one."
      end
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.run %W(cmd.name yy foo)
  cmd.run %W(cmd.name yy step)
  cmd.run %W(cmd.name)
  cmd.run %W(cmd.name yy next)
end
