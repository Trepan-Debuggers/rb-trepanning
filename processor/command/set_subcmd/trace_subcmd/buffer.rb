# -*- coding: utf-8 -*-
require_relative %w(.. .. base subsubcmd)
require_relative %w(.. trace)
class Debugger::SubSubcommand::SetTraceBuffer < Debugger::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP         = 
"set trace buffer [on|off|1|0]

Set saving trace events in a buffer
"
    MIN_ABBREV   = 'b'.size  
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set trace buffer)
  end

  def run(args)
    super
    if settings[:tracebuffer]
      # @proc.start_capture
      @proc.unconditional_prehooks.insert_if_new(-1, *@proc.tracebuf_hook)
    else
      @proc.unconditional_prehooks.delete_by_name('tracebuffer')
      # @proc.stop_capture
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  trace_cmd     = Debugger::SubSubcommand::SetTrace.new(dbgr.core.processor, 
                                                        set_cmd)

  # FIXME: remove the 'join' below
  cmd_name      = Debugger::SubSubcommand::SetTraceBuffer::PREFIX.join('')
  subcmd        = Debugger::SubSubcommand::SetTraceBuffer.new(set_cmd.proc, 
                                                              trace_cmd,
                                                              cmd_name)
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger

  subcmd.run([cmd_name])
  %w(off on 1 0).each { |arg| subcmd.run([cmd_name, arg]) }

end

