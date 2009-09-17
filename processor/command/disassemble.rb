# -*- coding: utf-8 -*-
require_relative 'base_cmd'
require_relative %w(.. .. lib disassemble)

class Debugger::Command::DisassembleCommand < Debugger::Command
    include Debugger::Disassemble

  unless defined?(HELP)
    HELP = 
"disassemble [thing]

With no argument, disassemble the current frame.  With a method, disassemble
that method.
"

    ALIASES       = %w(disas) # Note we will have disable
    CATEGORY      = 'data'
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
        ary = mark_disassembly(@proc.frame.iseq.disasm, @proc.frame.pc_offset)
        msg ary
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

  require_relative %w(.. mock)
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  MockDebugger::show_special_class_constants(cmd)
  def small_fn(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current, Thread::current)
    cmd.run [name]
  end
  small_fn(cmd, name)
end
