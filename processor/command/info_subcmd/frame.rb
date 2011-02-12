# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoFrame < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Show all information about the selected frame'
    MIN_ABBREV   = 'fr'.size # Note we have "info file"
    NEED_STACK   = true
  end

  def run(args)
    frame = @proc.frame
    section "Frame #{frame.method}"
    msg "  Line: %s" % @proc.frame_line
    msg "  %s: %s" % frame.source_container
    msg "  PC offset: %d" % frame.pc_offset
    msg "  argc: %d arity: %d" % [frame.argc, frame.arity]
    msg "  Type: %s" % frame.type
    if @proc.event == 'return'
      msg("  Return value class: #{@proc.frame.sp(1).class}")
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  dbgr, cmd = MockDebugger::setup('info')
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoFrame, false)
  cmd.run(cmd.prefix)
end
