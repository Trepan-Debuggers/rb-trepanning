# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../trace'
class Trepan::SubSubcommand::SetTraceVar < Trepan::SubSubcommand
  unless defined?(HELP)
    HELP         = <<-EOH
#{CMD} var GLOBAL_VARIABLE

The debugger calls 'trace_var' to trace changes to the value of
GLOBAL_VARIABLE.  Note in contrast to other events, stopping for
variable tracing occurs *after* the event, not before.

See also 'set events'.
    EOH

    MIN_ABBREV   = 'v'.size  
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = "Set to display trace a global variable."
    PREFIX       = %w(set trace var)
  end

  def run(args)
    if args.size == 2
      traced_var = args[1]
      unless traced_var[0] == '$'
        errmsg "Expecting a global variable to trace, got: #{traced_var}"
        return
      end
      trace_var(traced_var, @proc.core.method(:trace_var_processor))
      msg("Tracing variable #{traced_var}.")
      return
    else
      errmsg "Expecting two arguments, got #{args.size}"
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
  set_cmd.proc.send('frame_initialize')
  testcmdMgr = Trepan::Subcmd.new(set_cmd)
  cmd_name   = Trepan::SubSubcommand::SetTraceVar::PREFIX.join('')
  setx_cmd   = Trepan::SubSubcommand::SetTraceVar.new(set_cmd.proc, 
                                                      set_cmd,
                                                      cmd_name)
  setx_cmd.run([])
  # require_relative '../../../../lib/bdbgr'
  # dbgr = Trepan.new
  # dbgr.debugger
  eval('set_cmd.proc.frame_setup(RubyVM::ThreadFrame::current); setx_cmd.run([])')

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end

