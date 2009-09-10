# -*- coding: utf-8 -*-
# A base class for debugger subcommands.
#
# Note: don't end classname with Command (capital C) since main
# will think this a command name like QuitCommand 
#                                         ^

# Base Class for Debugger subcommands. We pull in some helper
# functions for command from module cmdfns.

require_relative 'base_cmd'

class Debugger

  class Subcommand  < Command

    NotImplementedMessage = 
      "This method must be overriden in a subclass" unless 
      defined?(NotImplementedMessage)

    attr_reader :name

    unless defined?(IN_LIST)
      IN_LIST    = true  # Show item in help list of commands
      RUN_CMD    = true  # Run subcommand for those subcommands like "show"
      # which append current settings to list output.
      MIN_ABBREV = 1
      NEED_STACK = false
      NAME       = 'your_command_name'
    end


    # cmd contains the command object that this
    # command is invoked through.  A debugger field gives access to
    # the stack frame and I/O.
    def initialize(cmd)
      @cmd = cmd

      # Convenience class access. We don't expect that any of these
      # will change over the course of the program execution like
      # errmsg(), msg(), and msg_nocr() might. (See the note below
      # on these latter 3 methods.)
      # 
      @core     = cmd.core
      @proc     = cmd.proc
      # @dbgr = cmd.dbgr

      # By default the name of the subcommand will be the name of the
      # last part of module (e.g. "args" in "infos.args" or "basename"
      # in "shows.basename"). However it *is* possible for one to change
      # that -- perhaps one may want to put several subcommands into 
      # a single file. So in those cases, one will have to set @name
      # accordingly by other means.
      @name  = my_const(:NAME).to_sym
      
    end

    # Convenience short-hand for @proc.confirm
    def confirm(msg, default=false)
      return(@proc.confirm(msg, default))
    end

    # Set a Boolean-valued debugger setting. 
    def run_set_bool(args, default=true)
      args = ['on'] if args.empty?
      begin
        settings[@name] = @proc.get_onoff(args[0])
        run_show_bool
      rescue NameError, TypeError
      end
    end

    # set an Integer-valued debugger setting. 
    def run_set_int(arg, msg_on_error, min_value=nil, max_value=nil)
      if arg.strip.empty?
        errmsg('You need to supply a number.')
        return
      end
      val = @proc.get_an_int(arg, 
                             :max_value => max_value,
                             :min_value => min_value, 
                             :msg_on_error => msg_on_error
                             )
      if val
        settings[@name] = val
        run_show_int
      end
    end

    # Generic subcommand showing a boolean-valued debugger setting.
    def run_show_bool(what=nil)
      val = show_onoff(settings[@name])
      what = @name unless what
      msg("%s is %s." % [what, val])
    end

    # Generic subcommand integer value display
    def run_show_int(what=nil)
      val = settings[@name]
      what = @name unless what
      msg("%s is %d." % [what, val])
    end

    # Generic subcommand value display. Pass in a hash which may
    # which optionally contain:
    #
    #   :name - the String name of key in settings to use. If :value
    #           (described below) is set, then setting :name does
    #           nothing.
    #
    #   :what - the String name of what we are showing. If none is
    #           given, then we use the part of the SHORT_HELP string.
    # 
    #   :value - a String value associated with "what" above. If none
    #            is given, then we pick up the value from settings.
    # 
    def run_show_val(opts={})
      what = opts.member?(:what)  ? opts[:what]  : string_in_show
      name = opts.member?(:name)  ? opts[:name]  : @name
      val  = opts.member?(:value) ? opts[:value] : settings[name] 
      msg("%s is %s." % [what, val])
    end

    def settings
      @proc.settings
    end
        

    # Return 'on' for true and 'off' for false, and ?? for anything else.
    def show_onoff(bool)
      case(bool)
      when true;  return 'on'
      when false; return 'off'
      when nil;   return 'unset'
      else        return '??'
      end
    end

    def string_in_show
      my_const(:SHORT_HELP)['Show '.size .. -1].capitalize
    end

    def summary_help(subcmd_name)
      msg_nocr("%-12s: %s" % [subcmd_name, my_const(:SHORT_HELP)])
    end
    
  end

  class SetBoolSubcommand < Subcommand
    def run(args)
      run_set_bool(args)
    end

  end

  class ShowBoolSubcommand < Subcommand
    def run(args)
      run_show_bool(string_in_show)
    end
  end

  class ShowIntSubcommand < Subcommand
    def run(args)
      if self.respond_to?(:short_help)
        doc = short_help
      else
        doc = my_const(:HELP)[5..-2].capitalize
      end
      run_show_int(doc)
    end
  end

  # FIXME: Put in another file? 
  class SubSubcommand  < Subcommand
    def initialize(cmd, parent)
      @cmd    = cmd
      @parent = parent
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. mock)
  dbgr = MockDebugger::MockDebugger.new(nil)
  cmds = dbgr.core.processor.commands
  p cmds.keys
  subcmd = Debugger::Subcommand.new(cmds['exit'])
  def subcmd.msg(message)
    puts message
  end
  def subcmd.errmsg(message)
    puts message
  end
  p subcmd.settings
  p subcmd.show_onoff(subcmd.settings[:autoeval])
  subcmd.run_set_int('', 'Just a test')
end
