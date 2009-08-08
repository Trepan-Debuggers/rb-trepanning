# -*- coding: utf-8 -*-
# A base class for debugger commands.
#
# This file is a module in this directory that isn't a real command
# and commands.py needs to take care to avoid instantiating this class
# and storing it as a list of known debugger commands.

# Note: don't end classname with Command (capital C) since main
# will think this a command name like QuitCommand 
#                                         ^

# Base Class for Debugger subcommands. We pull in some helper
# functions for command from module cmdfns.
class Debugger

  class Subcommand

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
      @proc     = cmd.proc
      @core     = cmd.core
      # @dbgr = cmd.dbgr

      # By default the name of the subcommand will be the name of the
      # last part of module (e.g. "args" in "infos.args" or "basename"
      # in "shows.basename"). However it *is* possible for one to change
      # that -- perhaps one may want to put several subcommands into 
      # a single file. So in those cases, one will have to set @name
      # accordingly by other means.
      @name  = self.class.const_get(:NAME).to_sym
      
      def summary_help(subcmd_name)
        msg_nocr("%-12s: %s" % [subcmd_name, self.class.const_get(:SHORT_HELP)])
      end

    end

    # Convenience short-hand for @dbgr.intf.confirm
    def confirm(msg, default=false)
      return(@dbgr.intf[-1].confirm(msg, default))
    end

    # Note for errmsg, msg, and msg_nocr we don't want to simply make
    # an assignment of method names like @msg = @dbgr.intf.msg,
    # because we want to allow the interface (intf) to change 
    # dynamically. That is, the value of @dbgr may change
    # in the course of the program and if we made such an method assignemnt
    # we wouldn't pick up that change in our @msg

    # Convenience short-hand for @dbgr.intf[-1].errmsg
    def errmsg(msg)
      @proc.errmsg(msg)
      # @dbgr.intf[-1].errmsg(msg)
    end
    
    # Convenience short-hand for @dbgr.intf[-1].msg
    def msg(msg)
      @proc.msg(msg)
      # @dbgr.intf[-1].msg(msg)
    end
    
    # Convenience short-hand for @dbgr.intf[-1].msg_nocr
    def msg_nocr(msg)
      @proc.msg_nocr(msg)
      # @dbgr.intf[-1].msg_nocr(msg)
    end

    # The method that implements the dbgr command.
    def run
      raise RuntimeError, NotImplementedMessage
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

    # Return 'on' for true and 'off' for false, and ?? for anything else.
    def show_onoff(bool)
      case(bool)
      when true;  return 'on'
      when false; return 'off'
      when nil;   return 'unset'
      else       return '??'
      end
    end

    def settings
      @cmd.proc.settings
    end

  end

  class SetBoolSubcommand < Subcommand
    def run(args)
      run_set_bool(args)
    end

  end

  class ShowBoolSubcommand < Subcommand
    def run(args)
      doc = self.class.const_get(:HELP)[5..-1].capitalize.split('\n')[0].chomp('.')
      run_show_bool(doc)
    end
  end

  class ShowIntSubcommand < Subcommand
    def run(args)
      if self.respond_to?(:short_help)
        doc = short_help
      else
        doc = self.class.get_const(:HELP)[5..-1].capitalize
      end
      run_show_int(doc)
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative File.join(%w(.. mock))
  dbgr = MockDebugger.new
  cmds = dbgr.core.processor.instance_variable_get('@commands')
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
