# -*- coding: utf-8 -*-
require_relative 'cmd'
require_relative %w(.. .. subcmd)

class Debugger::SubcommandMgr < Debugger::Command

  unless defined?(CATEGORY)
    CATEGORY      = 'status'
    MIN_ARGS      = 0
    MAX_ARGS      = nil
    NAME          = '?' # FIXME: Need to define this, but should 
                        # pick this up from class/file name.
    NEED_STACK    = false
  end

  attr_accessor :subcmds   # Array of instaniated Debugger::Subcommand objects
  attr_reader   :name      # Name of command
  attr_reader   :last_args # Last arguments seen

  # Initialize show subcommands. Note: instance variable name
  # has to be setcmds ('set' + 'cmds') for subcommand completion
  # to work.
  def initialize(proc)
    @name    = obj_const(self, :NAME)
    @subcmds = Debugger::Subcmd.new(self)
    @proc    = proc
    load_debugger_subcommands(@name, self)
  end

  # Create an instance of each of the debugger subcommands. Commands
  # are found by importing files in the directory 'name' + '_sub'. Some
  # files are excluded via an array set in initialize.  For each of
  # the remaining files, we import them and scan for class names
  # inside those files and for each class name, we will create an
  # instance of that class. The set of DebuggerCommand class instances
  # form set of possible debugger commands.
  def load_debugger_subcommands(name, parent)

    # Initialization
    cmd_names     = []
    subcmd_names  = []
    cmd_dir = File.dirname(__FILE__)
    subcmd_dir = File.join(cmd_dir, '..', name + '_subcmd')
    files = Dir.glob(File.join(subcmd_dir, '*.rb'))
    files.each do |rb| 
      basename = File.basename(rb, '.rb')
      if File.directory?(File.join(File.dirname(rb), basename + '_subcmd'))
        subcmd_names << name.capitalize + basename.capitalize
      else
        cmd_names << name.capitalize + basename.capitalize
      end
      require rb
    end if File.directory?(subcmd_dir)

    subcommands = {}
    cmd_names.each do |name|
      next unless Debugger::Subcommand.constants.member?(name.to_sym)
      subcmd_class = "Debugger::Subcommand::#{name}.new(self)"
      cmd = self.instance_eval(subcmd_class)
      cmd_name = cmd.name
      @subcmds.add(cmd)
    end
    subcmd_names.each do |name|
      next unless Debugger::SubSubcommand.constants.member?(name.to_sym)
      subcmd_class = "Debugger::SubSubcommand::#{name}.new(self, parent)"
      cmd = self.instance_eval(subcmd_class)
      cmd_name = cmd.name
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

  def run(args)
    @last_args = args
    if args.size < 2
      # We were given cmd without a subcommand; cmd is something
      # like "show", "info" or "set". Generally this means list
      # all of the subcommands.
      msg("List of %s commands (with minimum abbreviation parenthesis):" % 
          obj_const(self, :NAME))
      @subcmds.list.each do |subcmd_name|
        # Some commands have lots of output.
        # they are excluded here because 'in_list' is false.
        summary_help(@subcmds.subcmds[subcmd_name])
      end
      return false
    end

    subcmd_prefix = args[1]
    # We were given: cmd subcmd ...
    # Run that.
    subcmd = @subcmds.lookup(subcmd_prefix)
    if subcmd
      subcmd.run(args)
    else
      undefined_subcmd(@name, subcmd_prefix)
    end
  end

  def obj_const(obj, name); obj.class.const_get(name) end

  def summary_help(subcmd)
    # Set class constant SHORT_HELP to be the first line of HELP
    # unless it has been defined in the class already.
    # The below was the simplest way I could find to do this since
    # we are the super class but want to set the subclass's constant.
    # defined? didn't seem to work here.
    c = subcmd.class.constants
    if c.member?(:HELP) and !c.member?(:SHORT_HELP)
      short_help = subcmd.class.const_get('HELP').split("\n")[0].chomp('.')
      subcmd.class.const_set('SHORT_HELP', short_help)
    end
                     
    msg('%s (%d) -- %s' %
        [obj_const(subcmd, :NAME), 
         obj_const(subcmd, :MIN_ABBREV),
         obj_const(subcmd, :SHORT_HELP)])
  end

  # Error message when subcommand asked for but doesn't exist
  def undefined_subcmd(cmd, subcmd)
    errmsg(('Undefined "%s" subcommand: "%s". ' + 
            'Try "help %s *".') % [cmd, subcmd, cmd])
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  cmd  = cmds['set']
  Debugger::SubcommandMgr.new(dbgr.core.processor)
  puts cmd.help(%w(help set))
  puts '=' * 40
  # require_relative %w(.. .. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  puts cmd.help(%w(help set *))
  puts '=' * 40
  puts cmd.help(%w(help set d.*))
end
