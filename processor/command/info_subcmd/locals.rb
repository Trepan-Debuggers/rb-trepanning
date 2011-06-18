# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoLocals < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = 'Same thing as "info locals"'
    NEED_LOCALS   = true
   end

  def run(args)
    @proc.commands['info'].run(%w(info variables locals))
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  # ???
end
