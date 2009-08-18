# -*- coding: utf-8 -*-
require 'trace'
require_relative File.join(%w(.. base_subcmd))

class Debugger::Subcommand::ShowTraceset < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 'Show trace events we may stop on'
    MIN_ABBREV   = 'traces'.size
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = HELP
  end

  # FIXME: this really should be a subcommand of "set trace"
  def run(args)
    # FIXME:? this show events for all hooks while we want just ours?
    event_bitmasks = Trace.event_masks
    if event_bitmasks.empty?
      msg('No events trapped.')
    else
      msg('Trace events we may stop on:')
      event_bitmasks.each do |event_bitmask|
        msg("\t" + Trace.bitmask2events(event_bitmask).join(', '))
      end
    end
  end


end

if __FILE__ == $0
  # Demo it.
  require_relative File.join(%w(.. .. mock))
  require_relative File.join(%w(.. .. subcmd))
  dbgr = MockDebugger.new
  cmds = dbgr.core.processor.instance_variable_get('@commands')
  cmd = cmds['exit']
  subcommand = Debugger::Subcommand::ShowTraceset.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  puts
  subcommand.run([name])
end
