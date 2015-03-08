# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowConfirm < Trepan::ShowBoolSubcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
**#{PREFIX.join(' ')}**

Show confirm potentially dangerous operations setting.

See also:
---------

`set confirm`
EOH

    SHORT_HELP  = "Show confirm potentially dangerous operations setting"
    MIN_ABBREV = 'co'.size
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Trepan::Subcommand::ShowConfirm.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  subcommand.run_show_bool
  name = File.confirm(__FILE__, '.rb')
  subcommand.summary_help(name)
end
