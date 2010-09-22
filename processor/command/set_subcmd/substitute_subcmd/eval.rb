# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../frame'
require_relative '../../base/subsubcmd'
require_relative '../substitute'

class Trepan::SubSubcommand::SetSubstituteEval < Trepan::SubSubcommand
  unless defined?(HELP)
    HELP         = 
'set substitute eval [FRAME-NUM]

Causes lines in an EVAL frame to show up as we stop/step through them. 
The text fror the eval string comes from the current frame. 

FRAME-NUM is a relative frame number unless prefaced with an "=" which
indicates how many frames prior to move. The default is "0"
(alternatively "=0", the current frame.

This is largely done automatically anytime the debugger discovers an EVAL frame.
We have this here for completeness and just in case.
'
    MIN_ABBREV   = 'ev'.size  
    MAX_ARGS     = 1
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = 'Set eval string text of an filename'
    PREFIX       = %w(set substitute string)
  end

  def run(args)

    frame = 
      if args.size == 2
        absolute, count_str = 
          if '=' == args[1][0]
            [true, args[1][1..-1].dup]
          else
            [false, args[1]]
          end
        stack_size = @proc.top_frame.stack_size - @hide_level
        opts = {
        :msg_on_error => 
        "The 'eval' command argument must eval to an integer. Got: %s" % count_str,
        :min_value => -stack_size,
        :max_value => stack_size-1
      }
        count = @proc.get_an_int(count_str, opts)
        @proc.get_frame(count, absolute)[0]
      else
        @proc.frame
    end
    return unless frame
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
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  set_cmd.proc.send('frame_initialize')
  testcmdMgr = Trepan::Subcmd.new(set_cmd)
  cmd_name   = Trepan::SubSubcommand::SetSubstituteEval::PREFIX.join('')
  setx_cmd   = Trepan::SubSubcommand::SetSubstituteEval.new(set_cmd.proc, 
                                                            set_cmd,
                                                            cmd_name)
  setx_cmd.run([])
  # require_relative '../../../../lib/trepanning'
  # dbgr = Trepan.new(:set_restart => true)
  # dbgr.debugger
  eval('set_cmd.proc.frame_setup(RubyVM::ThreadFrame::current); setx_cmd.run([])')

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
