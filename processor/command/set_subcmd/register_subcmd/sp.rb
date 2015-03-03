# -*- coding: utf-8 -*-
# Copyright (C) 2010-2012, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

# Note: The class name has to end in Sp, not SP since it is
# auto-instantiated.
class Trepan::Subcommand::SetRegisterSp < Trepan::SubSubcommand
    unless defined?(HELP)
        NAME         = File.basename(__FILE__, '.rb')
        SHORT_HELP   = 'Set a stack pointer register'
        IN_LIST      = true
        MIN_ABBREV   = 'sp'.size
        PREFIX       = %w(set register #{name})
        HELP         = <<-EOH
**#{PREFIX.join(' ')}** *integer-expression*

Set VM stack-pointer register (SP) to *integer-expression*

Warning: this is potentially dangerous.

See also:
---------

`info register sp`, `set return`
EOH
  end

  def run(args)
      # FIXME handle c-return
      # unless %w(return c-return).member?(@proc.event)
      if args.size < 3
          errmsg "Too few arguments - the 'sp' command requires number and a value"
          return
      end
      msg_on_error = "set SP requires an integer index"
      index = @proc.get_an_int(args[1],
                               :msg_on_error => msg_on_error
                               )
      return unless index
      new_val_str = args[2..-1].join(' ')
      begin
          new_val = @proc.debug_eval(new_val_str)
      rescue StandardError, ScriptError => e
          return
      end
      msg("Old value was: %s" % @proc.frame.sp(index).inspect)
      @proc.frame.sp_set(index, new_val)
      msg("New value is: %s" % new_val.inspect)
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # # FIXME: DRY the below code
  # dbgr, cmd = MockTrepan::setup('set')
  # subcommand = Trepan::Subcommand::SetSp.new(cmd)
  # testcmdMgr = Trepan::Subcmd.new(subcommand)

  # def subcommand.msg(message)
  #   puts message
  # end
  # def subcommand.msg_nocr(message)
  #   print message
  # end
  # def subcommand.errmsg(message)
  #   puts message
  # end
  # subcommand.run(%w(20))
  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
