# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::SetHighlight < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
**#{PREFIX.join(' ')}** [**on**|**off**]

Set whether we use terminal highlighting.

See also:
---------

`show highlight`

EOH
    SHORT_HELP       = 'Set whether we use terminal highlighting'
    IN_LIST    = true
    MIN_ABBREV = 'hi'.size
  end

  def complete(prefix)
    Trepan::Complete.complete_token(%w(on off reset), prefix)
  end

  def run(args)
    if args.size == 3 && 'reset' == args[2]
      LineCache::clear_file_format_cache
      @proc.settings[:highlight] = :term
    else
      super
      @proc.settings[:highlight] = :term if @proc.settings[:highlight]
    end
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this again
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetHighlight, false)
  cmd.run(cmd.prefix + ['off'])
  cmd.run(cmd.prefix + ['ofn'])
  cmd.run(cmd.prefix)
  puts cmd.save_command
end
