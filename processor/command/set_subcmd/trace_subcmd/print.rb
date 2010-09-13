# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../trace'
class Debugger::SubSubcommand::SetTracePrint < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP         = 
"set trace print [on|off|1|0]

Set printing trace events."

    MIN_ABBREV   = 'p'.size  
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set trace print)
    SHORT_HELP   = 'Set print trace events'
  end

  def run(args)
    super
    if settings[:traceprint]
      @proc.unconditional_prehooks.insert_if_new(-1, *@proc.trace_hook)
    else
      @proc.unconditional_prehooks.delete_by_name('trace')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  trace_cmd     = Debugger::SubSubcommand::SetTrace.new(dbgr.core.processor, 
                                                        set_cmd)

  # FIXME: remove the 'join' below
  cmd_name      = Debugger::SubSubcommand::SetTracePrint::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetTracePrint.new(set_cmd.proc, 
                                                             trace_cmd,
                                                             cmd_name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger

  subcmd.run([cmd_name])
  %w(off on 1 0).each { |arg| subcmd.run([cmd_name, arg]) }
  puts subcmd.save_command()

end

