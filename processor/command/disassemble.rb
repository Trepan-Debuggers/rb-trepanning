# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. .. lib disassemble)

class Debugger::Command::DisassembleCommand < Debugger::Command
    include Debugger::Disassemble

  unless defined?(HELP)
    HELP = 
"disassemble [thing] [short]

With no argument, disassemble the current frame.  With a method, disassemble
that method. 
"

    ALIASES       = %w(disas) # Note we will have disable
    CATEGORY      = 'data'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = true
    SHORT_HELP    = 'Disassemble Ruby VM instructions'
  end

  # FIXME: put in processor/data.rb? 

  def marked_disassemble(iseq)
    iseq.child_iseqs.each do |iseq|
      ary = mark_disassembly(iseq.disasm_nochildren, 
                             @proc.frame.iseq.equal?(iseq),
                             @proc.frame.pc_offset)
      msg ary
    end
  end

  # Run command. 
  def run(args)

    obj = nil
    short = 
      if args.size > 1 && args[-1] == 'short'
        args.pop
        true
      else
        false
      end
    if args.size == 1
      # Form is: "disassemble" 
      if @proc.frame.type == 'CFUNC'
        errmsg "Can't handle C functions yet."
        return
      elsif @proc.frame.iseq
        marked_disassemble(@proc.frame.iseq)
        return
      end
    else
      iseq = @proc.method_iseq(args[1])
      marked_disassemble(iseq) if iseq
      return
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
  def small_fn(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    cmd.run [name]
  end
  small_fn(cmd, name)
end
