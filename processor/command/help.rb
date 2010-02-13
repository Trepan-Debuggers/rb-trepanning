require 'columnize'
require_relative %w(base cmd)
class Debugger::Command::HelpCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"help [command [subcommand]|expression]

Without argument, print the list of available debugger commands.

When an argument is given, it is first checked to see if it is command
name. 'help where' gives help on the 'where' debugger command.

If the environment variable $PAGER is defined, the file is
piped through that command.  You'll notice this only for long help
output.

Some commands like 'info', 'set', and 'show' can accept an
additional subcommand to give help just about that particular
subcommand. For example 'help info line' give help about the
info line command.

See also 'examine' and 'whatis'.
"


    ALIASES       = %w(?)
    CATEGORIES    = {
    'breakpoints' => 'Making the program stop at certain points',
    'data'        => 'Examining data',
    'files'       => 'Specifying and examining files',
    'running'     => 'Running the program', 
    'status'      => 'Status inquiries',
    'support'     => 'Support facilities',
    'stack'       => 'Examining the call stack'
    }
    CATEGORY      = 'support'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = false
    SHORT_HELP    = 'Print commands or give help for command(s)'
  end

  # List the command categories and a short description of each.
  def list_categories
    msg("Classes of commands:")
    CATEGORIES.keys.sort.each do |cat|
      msg("%-13s -- %s" % [cat, CATEGORIES[cat]])
    end
    final_msg = '
Type "help" followed by a class name for a list of commands in that class.
Type "help *" for the list of all commands.
Type "help REGEXP" for the list of commands matching /^#{REGEXP}/
Type "help CLASS *" for the list of all commands in class CLASS.
Type "help" followed by command name for full documentation.
'
    msg(final_msg)  
  end

  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      cmd_name = args[1]
      if cmd_name == '*'
        msg("All command names:")
        msg columnize_commands(@proc.commands.keys.sort)
      elsif CATEGORIES.member?(cmd_name)
        show_category(args[1], args[2..-1])
      elsif @proc.commands.member?(cmd_name) or @proc.aliases.member?(cmd_name)
        real_name = 
          if @proc.commands.member?(cmd_name) 
            cmd_name
          else
            @proc.aliases[cmd_name]
          end
        cmd_obj = @proc.commands[real_name]
        help_text = 
          cmd_obj.respond_to?(:help) ? cmd_obj.help(args) : 
          cmd_obj.class.const_get(:HELP)
        if help_text
          msg(help_text) 
          if cmd_obj.class.constants.member?(:ALIASES)
            msg "Aliases: #{cmd_obj.class.const_get(:ALIASES).join(', ')}"
          end
        end
      else 
        matches = @proc.commands.keys.grep(/^#{cmd_name}/).sort rescue []
        if matches.empty?
          errmsg("No commands found matching /^#{cmd_name}/. Try \"help\".")
        else
          msg("Command names matching /^#{cmd_name}/:")
          msg columnize_commands(matches.sort)
        end
      end
    else
      list_categories
    end
    return false  # Don't break out of cmd loop
  end

  # Show short help for all commands in `category'.
  def show_category(category, args)
      
    if args.size == 1 && args[0] == '*'
      msg("Commands in class %s:" % category)
      
      cmds = @proc.commands.keys.select do |cmd_name|
        category == @proc.commands[cmd_name].category
      end.sort

      width = settings[:width]
      return columnize_commands(cmds)
    end
        
    msg("%s." % CATEGORIES[category])
    msg("List of commands:\n")
    @proc.commands.keys.sort.each do |name|
      next if category != @proc.commands[name].category
      msg("%-13s -- %s" % [name, @proc.commands[name].short_help])
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)

  cmd.run %w(help help)
  puts '=' * 40
  cmd.run %w(help *)
  puts '=' * 40
  cmd.run %w(help fdafsasfda)
  puts '=' * 40
  cmd.run %w(help)
  puts '=' * 40
  cmd.run %w(help support)
  puts '=' * 40
  cmd.run %w(help support *)

  puts '=' * 40
  cmd.run %w(help s.*)
  puts '=' * 40
  cmd.run %w(help s<>)
end
