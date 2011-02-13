# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoFrame < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
#{CMD=PREFIX.join(' ')} 

Show information about the selected frame. The fields we list are:

* A source container and name (e.g. "file" or "string" and the value)
* The actual number of arguments passed in
* The 'arity' or permissible number of arguments passed it. -1 indicates
  variable number
* The frame "type", e.g. TOP, METHOD, BLOCK, EVAL, etc.
* The PC offfset we are currently at.

A backtrace shows roughly the same information in a more compact form.

See also: backtrace
    EOH
    MIN_ABBREV   = 'fr'.size # Note we have "info file"
    NEED_STACK   = true
    SHORT_HELP   = 'Show information about the selected frame'
  end

  def run(args)
    frame = @proc.frame
    section "Frame #{frame.method}"
    msg "  %-6s: %s" % frame.source_container
    msg "  line  : %s" % @proc.frame_line
    msg "  argc  : %d" % frame.argc
    msg "  arity : %d" % frame.arity
    msg "  type  : %s" % frame.type
    msg "  offset: %d" % frame.pc_offset
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
