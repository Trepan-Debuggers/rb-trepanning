# -*- coding: utf-8 -*-
require_relative 'base_cmd'

class Debugger::Command::DisassembleCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"disassemble [thing]

With no argument, disassemble the current frame.  With a method, disassemble
that method.
"

    ALIASES       = %w(disas) # Note we will have disable
    CATEGORY      = 'data'
    MIN_ARGS      = 0  # Need at least this many
    MAX_ARGS      = 1  # Need at most this many
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = true
    SHORT_HELP    = 'Disassemble Ruby VM instructions'
  end
  
  # Run command. 
  def run(args)

    obj = nil
    if args.size == 1
      # Form is: "disassemble" 
      if @proc.frame.type == 'CFUNC'
        errmsg "Can't handle C functions yet."
        return
      elsif @proc.frame.iseq
        msg(@proc.frame.iseq.disasm)
        return
      end
    else
      thingy = args[1]
      if @proc.debug_eval("#{thingy}.respond_to?(:disasm)")
        msg @proc.debug_eval("#{thingy}.disasm")
        return
      end
    end
    errmsg("Sorry can't handle right now.")
  end
end

if __FILE__ == $0
  # Demo it.
  require 'thread_frame'
  # FIXME: do more of the below setup in mock
  require_relative %w(.. mock)
  dbgr = MockDebugger.new

  cmds = dbgr.core.processor.instance_variable_get('@commands')
  name = File.basename(__FILE__, '.rb')
  cmd = cmds[name]
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current, Thread::current)
  cmd.run [name]
end
