# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'rubygems'; require 'require_relative'
require_relative '../base/subcmd'

class Trepan::Subcommand::SetReload < Trepan::SetBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    SHORT_HELP = "Set whether to reread source text when it changes."
    IN_LIST    = true
    MIN_ABBREV = 're'.size
    HELP         = <<-EOH
#{CMD} {on|off}

Source text is cached on the first read. This ensures that if you
change the source text after the debugged program is runnning you will
still see the source code as pertains to the running program rather
than what is in the filesystem.

However sometimes this may not be what you want. In particular in running
Ruby on Rails in development, Rails will also detect file changes and
will reload the source code. So here the debugger will be out of sync.

Set this to true, and the debugger will notice such changes and reread
the source text when it discovers it has changed.

See also "info source" and "info files" and note the SHA1
and file modification time.
    EOH
  end
end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this agin
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::SetReload, false)
  cmd.run(cmd.prefix + ['off'])
  cmd.run(cmd.prefix + ['ofn'])
  cmd.run(cmd.prefix)
  puts cmd.save_command
end
