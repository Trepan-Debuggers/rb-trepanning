# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/complete'

class Trepan::Subcommand::InfoMacro < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-HELP
#{CMD} 
#{CMD} *
#{CMD} MACRO1 [MACRO2 ..]

In the first form a list of the existing macro names are shown
in column format.

In the second form, all macro names and their definitions are shown.

In the last form the only definitions of the given macro names is shown.
    HELP
    SHORT_HELP = "Info defined macros"
    MIN_ABBREV = 'mac'.size
  end

  def complete(prefix)
    Trepan::Complete.complete_token(@proc.macros.keys + %w(*), prefix)
  end

  def run(args)
    if args.size > 2
      macro_names = 
        if args.size == 3 && '*' == args[2] 
          @proc.macros.keys
        else
          args[2..-1]
        end        
      macro_names.each do |macro_name|
        if @proc.macros.member?(macro_name)
          section "#{macro_name}:"
          string = @proc.macros[macro_name][1]
          msg "  #{@proc.ruby_format(string)}", {:unlimited => true}
        else
          errmsg '%s is not a defined macro' % macro_name
        end
      end
    elsif @proc.macros.empty?
      msg 'No macros defined.'
    else
      msg columnize_commands(@proc.macros.keys.sort)
    end
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this again
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoMacro)
  cmd.run(cmd.prefix + %w(u foo))
end
