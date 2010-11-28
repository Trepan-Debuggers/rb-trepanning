# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../trace'
class Trepan::SubSubcommand::SetTraceBuffer < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP         = 
"set trace buffer [on|off|1|0]

Set saving trace events in a buffer
"
    MIN_ABBREV   = 'b'.size  
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set trace buffer)
    SHORT_HELP   = 'Set saving trace events in a buffer'
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
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  trace_cmd     = Trepan::SubSubcommand::SetTrace.new(dbgr.core.processor, 
                                                      set_cmd)

  # FIXME: remove the 'join' below
  cmd_name      = Trepan::SubSubcommand::SetTraceBuffer::PREFIX.join('')
  subcmd        = Trepan::SubSubcommand::SetTraceBuffer.new(set_cmd.proc, 
                                                            trace_cmd,
                                                            cmd_name)
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new
  # dbgr.debugger

  subcmd.run([cmd_name])
  %w(off on 1 0).each { |arg| subcmd.run([cmd_name, arg]) }
  puts subcmd.save_command()

end

