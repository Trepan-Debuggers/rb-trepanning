# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::HelpCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [command [subcommand]|expression]

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
    HELP

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
    NEED_STACK    = false
    SHORT_HELP    = 'Print commands or give help for command(s)'
  end

  # List the command categories and a short description of each.
  def list_categories
    section 'Classes of commands:'
    CATEGORIES.keys.sort.each do |cat|
      msg("%-13s -- %s" % [cat, CATEGORIES[cat]])
    end
    final_msg = '
Type "help" followed by a class name for a list of commands in that class.
Type "help syntax" for information on debugger command syntax.
Type "help *" for the list of all commands.
Type "help all" for the list of all commands.
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
        section 'All command names:'
        msg columnize_commands(@proc.commands.keys.sort)
      elsif cmd_name =~ /^syntax$/i
        show_command_syntax
      elsif cmd_name =~ /^all$/i
        CATEGORIES.sort.each do |category|
          show_category(category[0], [])
        end
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
          if cmd_obj.class.constants.member?(:ALIASES) and
              args.size == 2
            msg "Aliases: #{cmd_obj.class.const_get(:ALIASES).join(', ')}"
          end
        end
      else 
        matches = @proc.commands.keys.grep(/^#{cmd_name}/).sort rescue []
        if matches.empty?
          errmsg("No commands found matching /^#{cmd_name}/. Try \"help\".")
        else
          section "Command names matching /^#{cmd_name}/:"
          msg columnize_commands(matches.sort)
        end
      end
    else
      list_categories
    end
  end

  # Show short help for all commands in `category'.
  def show_category(category, args)
      
    if args.size == 1 && args[0] == '*'
      section "Commands in class %s:" % category
      
      cmds = @proc.commands.keys.select do |cmd_name|
        category == @proc.commands[cmd_name].category
      end.sort
      width = settings[:maxwidth]
      return columnize_commands(cmds)
    end
        
    msg('')
    section "Command class: %s" % category
    msg('')
    @proc.commands.keys.sort.each do |name|
      next if category != @proc.commands[name].category
      msg("%-13s -- %s" % [name, @proc.commands[name].short_help])
    end
  end

  def show_command_syntax
    section "Debugger command syntax"
    msg <<-EOS
Command command syntax is very simple-minded. 

If a line starts with #, the command is ignored. 
If a line starts with !, the line is eval'd. 

If the command you want eval'd uses the Ruby ! initally, add that
after the first !.

Commands are split at whereever ;; appears. This process disregards
any quotes or other symbols that have meaning in Ruby. The strings
after the leading command string are put back on a command queue. 

Within a single command, tokens are then white-space split. Again,
this process disregards quotes or symbols that have meaning in Ruby.

The first token is then looked up in the debugger command table and
then the debugger alias table. If a match is found the command name
and arguments are dispatched to the command object that process the
command.

If the command is not found and "auto eval" is set on, then the
command is eval'd in the context that the program is currently stopped
at. If "auto eval" is not set on, then we display an error message
that the entered string is "undefined".

If you want irb-like command-processing, it's possible to go into an
irb shell with the "irb" command. It is also possible to arrange going
into an irb shell every time you enter the debugger.

Examples:

# This line does nothing. It is a comment
s    # by default, this is an alias for the "step" command
!s   # shows the value of variable step. 
!!s  # Evaluates !s (or "not s"). The first ! is indicates evaluate.
info program;; list # Runs two commands "info program" and "list"
pr  "hi ;;-)"  # Syntax error since ;; splits the line and " is not closed.
!puts "hi ;;-)" # One way to do the above.

See also "alias", "irb", "set auto eval", and "set auto irb".
    EOS
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup

  cmd.run %W(#{cmd.name} help)
  puts '=' * 40
  cmd.run %W(#{cmd.name} *)
  puts '=' * 40
  cmd.run %W(#{cmd.name} fdafsasfda)
  puts '=' * 40
  cmd.run [cmd.name]
  puts '=' * 40
  cmd.run %W(#{cmd.name} support)
  puts '=' * 40
  cmd.run %W(#{cmd.name} support *)
  puts '=' * 40
  cmd.run %W(#{cmd.name} s.*)
  puts '=' * 40
  cmd.run %W(#{cmd.name} s<>)
end
