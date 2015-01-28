# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoProgram < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = 'Information about debugged program and its environment'
    MIN_ARGS     = 0
    MAX_ARGS     = 0
    MIN_ABBREV   = 'pr'.size
    NEED_STACK   = true
  end

  def run(args)
    frame = @proc.frame
    m = 'Program stop event: %s' % @proc.event
    m +=
      if frame.iseq
        '; PC offset %d of instruction sequence: %s' %
          [frame.pc_offset, frame.iseq.label]
      else
        '.'
      end
    msg m
    if 'return' == @proc.event
      msg 'R=> %s' % @proc.frame.sp(1).inspect
    elsif 'raise' == @proc.event
      msg @proc.core.hook_arg.inspect if @proc.core.hook_arg
    end

    if @proc.brkpt
      msg('It is stopped at %sbreakpoint %d.' %
          [@proc.brkpt.temp? ? 'temporary ' : '',
           @proc.brkpt.id])
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoProgram, false)
  cmd.run(cmd.prefix)
end
