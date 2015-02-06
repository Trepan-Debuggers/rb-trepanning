# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../command'
require_relative '../../subcmd'
require_relative '../../help'
require_relative '../../../app/complete'

class Trepan::SubcommandMgr < Trepan::Command

  include Trepan::Help

  unless defined?(CATEGORY)
    CATEGORY      = 'status'
    MIN_ARGS      = 0
    MAX_ARGS      = nil
    NAME          = '?' # FIXME: Need to define this, but should
                        # pick this up from class/file name.
    NEED_STACK    = false
  end

  attr_accessor :subcmds   # Trepan::Subcmd
  attr_reader   :name      # Name of command
  attr_reader   :last_args # Last arguments seen

  # Initialize show subcommands. Note: instance variable name
  # has to be setcmds ('set' + 'cmds') for subcommand completion
  # to work.
  def initialize(proc)
    @name    = obj_const(self, :NAME)
    @subcmds = Trepan::Subcmd.new(self)
    @proc    = proc
    load_debugger_subcommands(self)
  end

  # Create an instance of each of the debugger subcommands. Commands
  # are found by importing files in the directory 'name' + '_sub'. Some
  # files are excluded via an array set in initialize.  For each of
  # the remaining files, we import them and scan for class names
  # inside those files and for each class name, we will create an
  # instance of that class. The set of TrepanCommand class instances
  # form set of possible debugger commands.
  def load_debugger_subcommands(parent)

    # Initialization
    cmd_names     = []
    subcmd_names  = []
    cmd_basenames = []
    cmd_dir = File.dirname(__FILE__)
    subcmd_dir = File.join(cmd_dir, '..', name + '_subcmd')
    files = Dir.glob(File.join(subcmd_dir, '*.rb'))
    files.each do |rb|
      basename = File.basename(rb, '.rb')
      if File.directory?(File.join(File.dirname(rb), basename + '_subcmd'))
        subcmd_names << name.capitalize + basename.capitalize
      else
        cmd_names << name.capitalize + basename.capitalize
        cmd_basenames << basename
      end
      require rb
    end if File.directory?(subcmd_dir)

    subcommands = {}
    cmd_names.each_with_index do |name, i|
      next unless Trepan::Subcommand.constants.member?(name.to_sym)
      subcmd_class = self.instance_eval("Trepan::Subcommand::#{name}")
      unless subcmd_class.const_defined?(:NAME)
        subcmd_class.const_set(:NAME, cmd_basenames[i])
      end
      unless subcmd_class.const_defined?(:PREFIX)
        subcmd_class.const_set(:PREFIX, %W(#{parent.name} #{cmd_basenames[i]}))
      end
      subcmd_new_str = "Trepan::Subcommand::#{name}.new(self)"
      cmd = self.instance_eval(subcmd_new_str)
      cmd_name = cmd.name
      @subcmds.add(cmd)
    end
    subcmd_names.each do |name|
      next unless Trepan::SubSubcommand.constants.member?(name.to_sym)
      subcmd_class = Trepan::SubSubcommand.const_get(name)
      begin
        cmd = subcmd_class.send(:new, self, parent)
      rescue Exception => exc
        puts "Subcmd #{name} in #{parent.name.inspect} is bad: #{exc}"
      end
      @subcmds.add(cmd)
    end
  end

  # Give help for a command which has subcommands. This can be
  # called in several ways:
  #        help cmd
  #        help cmd subcmd
  #        help cmd commands
  #
  #  Our shtick is to give help for the overall command only if
  #  subcommand or 'commands' is not given. If a subcommand is given and
  #  found, then specific help for that is given. If 'commands' is given
  #  we will list the all the subcommands.
  def help(args)
    if args.size <= 2
      # "help cmd". Give the general help for the command part.
      doc = my_const(:HELP)
      if doc
       return doc
      else
        errmsg('Sorry - author mess up. ' +
               'No help registered for command' +
               @name)
        return nil
      end
    end

    subcmd_name = args[2]

    if '*' == subcmd_name
      help_text = ["List of subcommands for command '%s':" % @name]
      help_text << columnize_commands(@subcmds.list)
      return help_text
    end

    # "help cmd subcmd". Give help specific for that subcommand.
    cmd = @subcmds.lookup(subcmd_name, false)
    if cmd
      if cmd.respond_to?(:help)
        return cmd.help(args)
      else
        doc = obj_const(cmd, :HELP)
        if doc
          return doc
        else
          errmsg('Sorry - author mess up. ' +
                 'No help registered for subcommand: ' +
                 subcmd_name + ', of command: ' +
                 @name)
          return nil
        end
      end
    else
      matches = @subcmds.list.grep(/^#{subcmd_name}/).sort
      if matches.empty?
        errmsg("No #{name} subcommands found matching /^#{subcmd_name}/. Try \"help\" #{@name}.")
        return nil
      elsif 1 == matches.size
        args[-1] = matches[0].to_s
        help(args)
      else
        help_text = ["Subcommands of \"#{@name}\" matching /^#{subcmd_name}/:"]
        help_text << columnize_commands(matches.sort)
        return help_text
      end
    end
  end

  # Return an Array of subcommands that can start with +arg+. If none
  # found we just return +arg+.
  # FIXME: Not used any more?
  def complete(prefix)
    Trepan::Complete.complete_token(@subcmds.subcmds.keys, prefix)
  end

  def complete_token_with_next(prefix)
    Trepan::Complete.complete_token_with_next(@subcmds.subcmds, prefix)
  end

  def run(args) # nodoc
    @last_args = args
    if args.size < 2 || args.size == 2 && args[-1] == '*'
      summary_list(obj_const(self, :NAME), @subcmds)
      return false
    end

    subcmd_prefix = args[1]
    # We were given: cmd subcmd ...
    # Run that.
    subcmd = @subcmds.lookup(subcmd_prefix)
    if subcmd
      if @proc.ok_for_running(subcmd, subcmd.class.const_get('CMD'),
                              args.size-2)
        subcmd.run(args)
      end
    else
      undefined_subcmd(@name, subcmd_prefix)
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('show')
  p cmd.complete('d')
  p cmd.subcmds.lookup('ar').prefix
  p cmd.subcmds.lookup('a')
end
