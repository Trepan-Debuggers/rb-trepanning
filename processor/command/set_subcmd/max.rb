# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subsubcmd'
require_relative '../base/subsubmgr'

class Trepan::SubSubcommand::SetMax < Trepan::SubSubcommandMgr
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP   = 'Set maximum length for things which may have unbounded size'
  end

  # def run(args)
  #   puts "foo"
  #   require 'trepanning'
  #   Trepan.debug
  #   super
  # end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr, parent_cmd = MockDebugger::setup('set', false)
  cmd              = Trepan::SubSubcommand::SetMax.new(dbgr.core.processor, 
                                                       parent_cmd)
  cmd.run(cmd.prefix + ['string', '30'])

  %w(s lis foo).each do |prefix|
    p [prefix, cmd.complete(prefix)]
  end
end
