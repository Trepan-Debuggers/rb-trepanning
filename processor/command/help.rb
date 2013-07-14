# Copyright (C) 2010, 2011, 2013 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../../app/complete'
class Trepan::Command::HelpCommand < Trepan::Command
  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [command [subcommand]|expression]

Without argument, print the list of available debugger commands.

When an argument is given, it is first checked to see if it is command
name. For example, 'help up' gives help on the 'up' debugger command.

If the environment variable $PAGER is defined, the file is
piped through that command.  You'll notice this only for long help
output.

Some commands like 'info', 'set', and 'show' can accept an
additional subcommand to give help just about that particular
subcommand. For example 'help info line' give help about the
info line command.
    HELP

    ALIASES       = %w(?)
    CATEGORIES    = {
    'breakpoints' => 'Making the program stop at certain points',
    'data'        => 'Examining data',
    'files'       => 'Specifying and examining files',
    'running'     => 'Running the program',
    'status'      => 'Status inquiries',
    'support'     => 'Support facilities',
    'stack'       => 'Examining the call stack',
    'syntax'      => 'Debugger command syntax'
    }
    CATEGORY      = 'support'
    NEED_STACK    = false
    SHORT_HELP    = 'Print commands or give help for command(s)'

    require 'thread_frame'
    ROOT_DIR = File.dirname(RubyVM::Frame.current.source_container[1])
    HELP_DIR      = File.join(ROOT_DIR, 'help')
  end

  def complete(prefix)
    matches = Trepan::Complete.complete_token(CATEGORIES.keys + %w(* all) +
                                              @proc.commands.keys, prefix)
    aliases = Trepan::Complete.complete_token_filtered(@proc.aliases, prefix,
                                                       matches)
    (matches + aliases).sort
  end

  def complete_token_with_next(prefix)
    complete(prefix).map do |cmd|
      [cmd, @proc.commands.member?(cmd) ? @proc.commands[cmd] : nil]
      # complete_method =
      #   if 'syntax' == cmd
      #     Syntax.new(syntax_files)
      #   else
      #     @proc.commands.member?(cmd) ? @proc.commands[cmd] : nil
      #   end
      # [cmd, complete_method]
    end
  end

  # List the command categories and a short description of each.
  def list_categories
    section 'Help classes:'
    CATEGORIES.keys.sort.each do |cat|
      msg("%-13s -- %s" % [cat, CATEGORIES[cat]])
    end
    final_msg = '
Type "help" followed by a class name for a list of help items in that class.
Type "help aliases" for a list of current aliases.
Type "help macros" for a list of current macros.
Type "help *" for the list of all commands, macros and aliases.
Type "help all" for a brief description of all commands.
Type "help REGEXP" for the list of commands matching /^#{REGEXP}/.
Type "help CLASS *" for the list of all commands in class CLASS.
Type "help" followed by a command name for full documentation.
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
        show_aliases  unless @proc.aliases.empty?
        show_macros unless @proc.macros.empty?
      elsif cmd_name =~ /^aliases$/i
        show_aliases
      elsif cmd_name =~ /^macros$/i
        show_macros
      elsif cmd_name =~ /^syntax$/i
        show_command_syntax(args)
      elsif cmd_name =~ /^all$/i
        CATEGORIES.sort.each do |category|
          show_category(category[0], [])
          msg('')
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
      elsif @proc.macros.member?(cmd_name)
        msg "#{cmd_name} is a macro which expands to:"
        msg "  #{@proc.macros[cmd_name]}", {:unlimited => true}
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

  def show_aliases
    section 'All alias names:'
    msg columnize_commands(@proc.aliases.keys.sort)
  end

  # Show short help for all commands in `category'.
  def show_category(category, args)

    if args.size == 1 && args[0] == '*'
      section "Commands in class %s:" % category

      cmds = @proc.commands.keys.select do |cmd_name|
        category == @proc.commands[cmd_name].category
      end.sort
      width = settings[:maxwidth]
      msg columnize_commands(cmds)
      return
    end

    section "Command class: %s" % category
    @proc.commands.keys.sort.each do |name|
      next if category != @proc.commands[name].category
      msg("%-13s -- %s" % [name, @proc.commands[name].short_help])
    end
  end

  def syntax_files
    @syntax_files ||= Dir.glob(File.join(HELP_DIR, '*.txt')).map do |txt|
      basename = File.basename(txt, '.txt')
    end
  end

  def show_command_syntax(args)
    if args.size == 2
      @syntax_summary_help ||= {}
      section "List of syntax help"
      syntax_files.each do |name|
        @syntax_summary_help[name] ||=
          File.open(File.join(HELP_DIR, "#{name}.txt")).readline.chomp
        msg "  %-8s -- %s" % [name, @syntax_summary_help[name]]
      end
    else
      args[2..-1].each do |name|
        if syntax_files.member?(name)
          @syntax_help ||= {}
          @syntax_help[name] =
            File.open(File.join(HELP_DIR, "#{name}.txt")).readlines[2..-1].join
          section "Debugger syntax for a #{name}:"
          msg @syntax_help[name]
        else
          errmsg "No syntax help for #{name}"
        end
      end
    end
  end

  def show_macros
    section 'All macro names:'
    msg columnize_commands(@proc.macros.keys.sort)
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
  puts '=' * 40
  p cmd.complete('br')
  p cmd.complete('un')
end
