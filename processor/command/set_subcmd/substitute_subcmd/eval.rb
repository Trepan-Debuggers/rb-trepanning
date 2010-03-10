# -*- coding: utf-8 -*-
require_relative %w(.. .. frame)
require_relative %w(.. .. base subsubcmd)
require_relative %w(.. substitute)

class Debugger::SubSubcommand::SetSubstituteEval < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 
'set substitute eval

Causes lines in an EVAL frame to show up as we stop/step through them. 
The text fror the eval string comes from the current frame.
'
    MIN_ABBREV   = 'ev'.size  
    MAX_ARGS     = 0
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = 'Set eval string text of an filename'
    PREFIX       = %w(set substitute string)
  end

  def run(args)
    frame = @proc.frame
    if 'EVAL' != frame.type
      errmsg "Current frame has to be of type EVAL, not #{frame.type}"
      return
    end

    unless frame.iseq
      errmsg "Can't get instruction sequence for frame."
      return
    end

    prev = frame.prev
    if 'CFUNC' != prev.type
      errmsg "Previous frame has to be of type CFUNC, not #{frame.type}"
      return
    end

    if 'eval' != prev.method
      errmsg "Previous frame has to be of type CFUNC, not #{frame.method}"
      return
    end
    @proc.frame_eval_remap
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  set_cmd.proc.send('frame_initialize')
  testcmdMgr = Debugger::Subcmd.new(set_cmd)
  cmd_name   = Debugger::SubSubcommand::SetSubstituteEval::PREFIX.join('')
  setx_cmd   = Debugger::SubSubcommand::SetSubstituteEval.new(set_cmd.proc, 
                                                              set_cmd,
                                                              cmd_name)
  setx_cmd.run([])
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  eval('set_cmd.proc.frame_setup(RubyVM::ThreadFrame::current); setx_cmd.run([])')

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
