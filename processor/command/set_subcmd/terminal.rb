# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetTerminal < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP       = 'Set whether we use terminal highlighting'
    IN_LIST    = true
    MIN_ABBREV = 'ba'.size
  end
  def run(args)
    if args.size == 3 && 'reset' == args[2]
      LineCache::clear_file_format_cache
    else
      super
      @proc.settings[:terminal] = :term if @proc.settings[:terminal]
    end
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this again
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetTerminal, false)
  cmd.run(cmd.prefix + ['off'])
  cmd.run(cmd.prefix + ['ofn'])
  cmd.run(cmd.prefix)
  puts cmd.save_command
end
