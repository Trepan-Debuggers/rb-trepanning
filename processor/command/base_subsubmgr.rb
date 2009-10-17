# -*- coding: utf-8 -*-
require 'columnize'
require_relative 'base_subcmd'
require_relative %w(.. subcmd)

class Debugger::SubSubcommandMgr < Debugger::Subcommand

  unless defined?(CATEGORY)
    CATEGORY      = 'status'
    MIN_ARGS      = 0
    MAX_ARGS      = nil
    NAME          = '?' # FIXME: Need to define this, but should 
                        # pick this up from class/file name.
    NEED_STACK    = false
  end

  attr_accessor :pname
  attr_accessor :subcmds  # Array of instantiated Debugger::Subcommand objects

  # Initialize show subcommands. Note: instance variable name
  # has to be setcmds ('set' + 'cmds') for subcommand completion
  # to work.
  def initialize(proc, parent)
    name     = obj_const(self, :NAME)
    @name    = name.to_sym
    @subcmds = Debugger::Subcmd.new(self)
    @parent  = parent
    @pname   = parent.name
    @proc    = proc

    # Set class constant SHORT_HELP to be the first line of HELP
    # unless it has been defined in the class already.
    # The below was the simplest way I could find to do this since
    # we are the super class but want to set the subclass's constant.
    # defined? didn't seem to work here.
    c = self.class.constants
    self.class.const_set('SHORT_HELP', 
                         self.class.const_get('HELP')) if
      c.member?(:HELP) and !c.member?(:SHORT_HELP)
    
    load_debugger_subsubcommands(name, self)
  end

  # Create an instance of each of the debugger subcommands. Commands
  # are found by importing files in the directory 'name' + 'sub'. Some
  # files are excluded via an array set in initialize.  For each of
  # the remaining files, we import them and scan for class names
  # inside those files and for each class name, we will create an
  # instance of that class. The set of DebuggerCommand class instances
  # form set of possible debugger commands.
  def load_debugger_subsubcommands(name, obj)

    # Initialization
    cmd_names     = []
    cmd_dir = File.dirname(__FILE__)
    subcmd_dir = File.join(cmd_dir, @pname + '_subcmd', name + '_subcmd')
    files = Dir.glob(File.join(subcmd_dir, '*.rb'))
    files.each do |rb| 
      cmd_names << name.capitalize + File.basename(rb, '.rb').capitalize
      require rb
    end if File.directory?(subcmd_dir)

    subcommands = {}
    cmd_names.each do |subname|
      cmd_name = "#{pname}#{subname.downcase}"
      subcmd_class = "Debugger::SubSubcommand::#{@pname.capitalize}#{subname}.new(self, @parent, '#{cmd_name}')"
      cmd = self.instance_eval(subcmd_class)
      @subcmds.add(cmd, cmd_name)
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
    if args.size <= 3
      # "help cmd". Give the general help for the command part.
      doc = self.class.const_get(:HELP)
      if doc
       return doc
      else
        errmsg('Sorry - author mess up. ' + 
               'No help registered for command' + 
               @name)
        return nil
      end
    end

    subcmd_name = args[3]
    subcmd_prefix  = args[1..2].join(' ')

    if '*' == subcmd_name
      # require_relative %w(.. .. rbdbgr)
      # dbgr = Debugger.new(:set_restart => true)
      # dbgr.debugger(:immediate => true)
      prefix_len =  my_const(:PREFIX).size
      help_text  = "List of subcommands for '%s':\n" % subcmd_prefix
      cmd_names = @subcmds.list.map{|c| c[prefix_len..-1]}
      help_text += Columnize::columnize(cmd_names, settings[:width], 
                                        '  ', true, true, lineprefix='  ').chomp
      return help_text
    end

    # "help cmd subcmd". Give help specific for that subcommand.
    cmd = @subcmds.lookup(args[1..3].join(''), false)
    if cmd
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
    else
      matches = @subcmds.list.grep(/^#{subcmd_name}/).sort
      if matches.empty?
        errmsg("No #{name} subcommands found matching /^#{subcmd_name}/. Try \"help\" #{@name}.")
        return nil
      else
        help_text = ["Subcommand(s) of \"#{@name}\" matching /^#{subcmd_name}/:"]
        help_text << columnize_commands(matches.sort)
        return help_text
      end
    end
  end

  def run(args)
    args = @parent.last_args
    if args.size < 3
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

    subcmd_prefix = args.join('')
    # We were given: cmd subcmd ...
    # Run that.
    subcmd = @subcmds.lookup(subcmd_prefix)
    if subcmd
      subcmd.run(args[2..-1])
    else
      undefined_subcmd(@name, subcmd_prefix)
    end
  end

  def obj_const(obj, name); obj.class.const_get(name) end

  def summary_help(subcmd)
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
  require_relative %w(.. mock)
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  cmd  = cmds['info']
  Debugger::SubSubcommandMgr.new(dbgr.core.processor, cmd)
  puts cmd.help(%w(help info registers))
  puts '=' * 40
  puts cmd.help(%w(help info registers *))
  puts '=' * 40
  # FIXME
  # require_relative %w(.. .. rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  # puts cmd.help(%w(help info registers p.*))
end
