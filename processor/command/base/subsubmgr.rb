# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'subcmd'
require_relative '../../subcmd'
require_relative '../../help'

class Trepan::SubSubcommandMgr < Trepan::Subcommand

  include Trepan::Help

  unless defined?(CATEGORY)
    CATEGORY      = 'status'
    MIN_ARGS      = 0
    MAX_ARGS      = nil
    NAME          = '?' # FIXME: Need to define this, but should 
                        # pick this up from class/file name.
    NEED_STACK    = false
  end

  attr_accessor :pname
  attr_accessor :subcmds  # Array of instantiated Trepan::Subcommand objects

  # Initialize show subcommands. Note: instance variable name
  # has to be setcmds ('set' + 'cmds') for subcommand completion
  # to work.
  # FIXME: do we need proc still? 
  def initialize(proc, parent)
    name     = obj_const(self, :NAME)
    @name    = name.to_sym
    @subcmds = Trepan::Subcmd.new(self)
    @parent  = parent
    @pname   = parent.name
    @proc    = parent.proc

    # Set class constant SHORT_HELP to be the first line of HELP
    # unless it has been defined in the class already.
    # The below was the simplest way I could find to do this since
    # we are the super class but want to set the subclass's constant.
    # defined? didn't seem to work here.
    c = self.class.constants
    self.class.const_set(:SHORT_HELP, 
                         self.class.const_get('HELP')) if
      c.member?(:HELP) and !c.member?(:SHORT_HELP)
    
    load_debugger_subsubcommands(name, self)
  end

  # Create an instance of each of the debugger subcommands. Commands
  # are found by importing files in the directory 'name' + 'sub'. Some
  # files are excluded via an array set in initialize.  For each of
  # the remaining files, we import them and scan for class names
  # inside those files and for each class name, we will create an
  # instance of that class. The set of TrepanCommand class instances
  # form set of possible debugger commands.
  def load_debugger_subsubcommands(name, obj)

    # Initialization
    cmd_names     = []
    cmd_dir = File.dirname(__FILE__)
    subcmd_dir = File.join(cmd_dir, '..', @pname + '_subcmd', name + '_subcmd')
    files = Dir.glob(File.join(subcmd_dir, '*.rb'))
    files.each do |rb| 
      cmd_names << name.capitalize + File.basename(rb, '.rb').capitalize
      require rb
    end if File.directory?(subcmd_dir)

    subcommands = {}
    cmd_names.each do |subname|
      cmd_name = "#{pname}#{subname.downcase}"
      subclass_name = "#{@pname.capitalize}#{subname}"
      next unless 
        Trepan::SubSubcommand.constants.member?(subclass_name.to_sym)
      cmd = self.instance_eval("Trepan::SubSubcommand::" + subclass_name + 
                               ".new(self, @parent, '#{cmd_name}')")
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

    prefix      = my_const(:PREFIX)
    subcmd_name = args[prefix.size+1]
    prefix_str  = prefix.join(' ')

    if '*' == subcmd_name
      help_text  = ["List of subcommands for '%s':" % prefix_str]
      cmd_names = @subcmds.list.map{|c| c[prefix_str.size-1..-1]}
      help_text << columnize_commands(cmd_names)
      return help_text
    end

    # "help cmd subcmd". Give help specific for that subcommand if
    # the command matches uniquely, or show a list of matching 
    # subcommands
    keyprefix_str  = prefix.join('')
    key_str        = keyprefix_str + subcmd_name
    cmd = @subcmds.lookup(key_str, false)
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
      matches = @subcmds.list.grep(/^#{key_str}/).sort
      if matches.empty?
        errmsg("No #{name} subcommands found matching /^#{subcmd_name}/. Try \"help #{@name}\".")
        return nil
      elsif 1 == matches.size
        args[-1] = matches[0].to_s[keyprefix_str.size..-1]
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
  def complete(prefix)
    prior = self.prefix.join('').size
    last_args = @subcmds.list.map{|str| str[prior..-1]}
    Trepan::Complete.complete_token(last_args, prefix)
  end

  def complete_token_with_next(prefix)
    Trepan::Complete.complete_token_with_next(@subcmds.subcmds, prefix,
                                              self.prefix.join(''))
  end

  def run(args)
    args = @parent.last_args if args.size == 0
    if args.size < 3 || args.size == 3 && args[-1] == '*'
      summary_list(obj_const(self, :NAME), @subcmds)
      return false
    end

    subcmd_prefix = args[0..2].join('')
    # We were given: cmd subcmd ...
    # Run that.
    subcmd = @subcmds.lookup(subcmd_prefix)
    if subcmd
      subcmd.run(args[2..-1])
    else
      undefined_subcmd(obj_const(self, :PREFIX).join(' '), args[-1])
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr = MockDebugger::MockDebugger.new
  cmds = dbgr.core.processor.commands
  cmd  = cmds['info']
  Trepan::SubSubcommandMgr.new(dbgr.core.processor, cmd)
  puts cmd.help(%w(help info registers))
  puts '=' * 40
  puts cmd.help(%w(help info registers *))
  puts '=' * 40
  # FIXME
  # require_relative '../../lib/trepanning'
  # Trepan.debug
  # puts cmd.help(%w(help info registers p.*))
end
