# -*- coding: utf-8 -*-
require_relative %w(base cmd)
require_relative %w(.. .. app disassemble)

class Debugger::Command::DisassembleCommand < Debugger::Command
    include Debugger::Disassemble

  unless defined?(HELP)
    HELP = 
"disassemble [thing] [full]

With no argument, disassemble the current frame.  With a method,
disassemble that method. '.' can be used to indicate the instruction
sequence for the current frame.

By default, just this instruction sequence is disassembled, not others
which may be accessed off of this one, including any catch table the
instruction sequence might have. If 'full' is given, all instruction
sequences are include.

Examples:

  disassemble 
  disas .       # Same as above
  disas . full  # At least the instruction sequence above but maybe more
  disas require_relative # disassemble method 'require_relative'
 "

    ALIASES       = %w(disas disassem) # Note we have disable
    CATEGORY      = 'data'
    NAME          = File.basename(__FILE__, '.rb')
    NEED_STACK    = true
    SHORT_HELP    = 'Disassemble Ruby VM instructions'
  end

  # FIXME: put in processor/data.rb? 

  def marked_disassemble(iseq, include_children)
    iseqs = include_children ? iseq.child_iseqs : [iseq]
      iseqs.each do |iseq|
      ary = mark_disassembly(iseq.disasm_nochildren, 
                             @proc.frame.iseq.equal?(iseq),
                             @proc.frame.pc_offset,
                             iseq.brkpts)
      msg ary
    end
  end

  # Run command. 
  def run(args)

    obj = nil
    include_children = 
      if args.size > 1 && args[-1] == 'full'
        args.pop
        true
      else
        false
      end
    if args.size == 1 || '.' == args[1]
      # Form is: "disassemble" or "disassemble ."
      if @proc.frame.type == 'CFUNC'
        errmsg "Can't handle C functions yet."
        return
      elsif @proc.frame.iseq
        marked_disassemble(@proc.frame.iseq, include_children)
        return
      end
    else 
      if !(matches = find_iseqs(ISEQS__, args[1])).empty?
        # FIXME: do something if there is more than one
        iseq = matches[0]
      else        
        iseq = @proc.object_iseq(args[1])
      end
      marked_disassemble(iseq, include_children) if iseq
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
  p = Proc.new do 
    |x,y| x + y
  end
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
  cmd.run([name, 'p'])
end
