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
  
  # Run command. 
  def run(args)

    obj = nil
    short = 
      if args.size > 1 && arg[-1] == 'short'
        args.pop
        true
      else
        short = false
      end
    if args.size == 1
      # Form is: "disassemble" 
      if @proc.frame.type == 'CFUNC'
        errmsg "Can't handle C functions yet."
        return
      elsif @proc.frame.iseq
        @proc.frame.iseq.child_iseqs.each do |iseq|
          ary = mark_disassembly(iseq.disasm_nochildren, 
                                 @proc.frame.iseq.equal?(iseq),
                                 @proc.frame.pc_offset)
          msg ary
        end
        return
      end
    else
      thingy = args[1]
      # FIXME: first try to get iseq so we can partition instruction
      # sequences better like we do above. And while we're at it, DRY
      # code with code above.
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
  def small_fn(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    cmd.run [name]
  end
  small_fn(cmd, name)
end
