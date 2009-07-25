require 'columnize'
require_relative 'base_cmd'
class Debugger::HelpCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"help [command [subcommand]|expression]

Without argument, print the list of available debugger commands.

When an argument is given, it is first checked to see if it is command
name. 'help exec' gives help on the ! command.

With the argument is an expression or object name, you get the same
help that you would get inside a Python shell running the built-in
help() command.

If the environment variable $PAGER is defined, the file is
piped through that command.  You'll notice this only for long help
output.

Some commands like 'info', 'set', and 'show' can accept an
additional subcommand to give help just about that particular
subcommand. For example 'help info line' give help about the
info line command.

See also 'examine' an 'whatis'.
"

    CATEGORIES = {
    'breakpoints' => 'Making the program stop at certain points',
    'data'        => 'Examining data',
    'files'       => 'Specifying and examining files',
    'running'     => 'Running the program', 
    'status'      => 'Status inquiries',
    'support'     => 'Support facilities',
    'stack'       => 'Examining the call stack'
    }

    CATEGORY     = 'support'
    MIN_ARGS     = 0    # Need at least this many
    MAX_ARGS     = nil  # Need at most this many
    
    # First entry is the name of the command. Any aliases for the
    # command follow.
    NAME_ALIASES = %w(help ?)
    NEED_STACK   = false
    
    SHORT_HELP  = 'Print commands or give help for command(s)'
  end

  # List all commands arranged in an aligned columns
  def columnize_all_commands
    commands = @proc.commands.keys.sort
    width = self.debugger.settings['width']
    self.msg(columnize(commands, width, '    '))
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      cmd_name = args[1]
      if cmd_name == '*'
      elsif @proc.commands.member?(cmd_name)
        cmd_obj = @proc.commands[cmd_name]
        puts cmd_obj.class.const_get(:HELP)
      else
        puts('Undefined command: "%s".  Try "help".' % 
                            cmd_name)
      end
    end
    return false  # Don't break out of cmd loop
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative File.join(%w(.. .. lib core))
  require_relative File.join(%w(.. cmdproc))
  core = Debugger::Core.new()
  dbg = Debugger::CmdProcessor.new(core)
  cmds = dbg.instance_variable_get('@commands')
  help_cmd = cmds['help']
  help_cmd.run %w(help help)
  puts '=' * 40
end
