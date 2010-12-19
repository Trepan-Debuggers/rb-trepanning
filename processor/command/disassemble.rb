# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../../app/disassemble'
require_relative '../../app/file'

class Trepan::Command::DisassembleCommand < Trepan::Command
    include Trepan::Disassemble
    include Trepanning

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    ALIASES       = %w(disas disassem) # Note we have disable
    CATEGORY      = 'data'
    HELP          = <<-HELP
#{NAME} [thing] [full]

With no argument, disassemble the current frame.  With a method,
disassemble that method. '.' can be used to indicate the instruction
sequence for the current frame.

By default, just this instruction sequence is disassembled, not others
which may be accessed off of this one, including any catch table the
instruction sequence might have. If 'full' is given, all instruction
sequences are include.

Examples:
  #{NAME} 
  #{NAME} .       # Same as above
  #{NAME} . full  # At least the instruction sequence above but maybe more
  #{NAME} require_relative # disassemble method 'require_relative'
    HELP

    NEED_STACK    = true
    SHORT_HELP    = 'Disassemble Ruby VM instructions'
  end

  # FIXME: put in processor/data.rb? 

  def marked_disassemble(iseq_param, include_children)
    iseqs = include_children ? iseq_param.child_iseqs : [iseq_param]
    iseqs.each do |iseq|
      ary = mark_disassembly(iseq.disasm_nochildren, 
                             @proc.frame.iseq.equal?(iseq),
                             @proc.frame.pc_offset,
                             iseq.brkpts, settings[:maxwidth])
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

  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  def small_fn(cmd, name)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
    cmd.run [name]
  end
  small_fn(cmd, cmd.name)
  p = Proc.new do 
    |x,y| x + y
  end
  cmd.proc.frame_setup(RubyVM::ThreadFrame::current)
  cmd.run([cmd.name, 'p'])
end
