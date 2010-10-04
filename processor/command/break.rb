# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../breakpoint'
require_relative '../../app/breakpoint'
class Trepan::Command::BreakCommand < Trepan::Command

  unless defined?(HELP)
    HELP = 
'break [line number|offset]

With a line number argument, set a break there in the current
instruction sequence.  With an offset (a number prefaced with an "O")
set a breakpoint at that instruction offset.

Examples:
   break
   break 10    # set breakpoint on line 10
   break o20   # set breakpoint VM Instruction Sequence offset 20
'

    ALIASES      = %w(b)
    CATEGORY     = 'breakpoints'
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Set a breakpoint'
  end

  # This method runs the command
  def run(args) # :nodoc
    # FIXME: handle more conditions
    # a line number
    if args.size == 1
      # usage is "break"  which means break right here
      # FIXME: should handle condition
      bp = @proc.breakpoint_offset(@proc.frame.pc_offset, 
                                   @proc.frame.iseq) 
    else
      position, iseq, use_offset, condition, name = 
        @proc.breakpoint_position(args[1..-1])
      return false unless position && iseq
      bp = 
        if use_offset
          @proc.breakpoint_offset(position, iseq)
        else
          @proc.breakpoint_line(position, iseq)
        end
    end
    if bp
      bp.condition = condition

      mess = "Breakpoint %d set at " % bp.id

      line_loc = "line %s in %s" % 
        [bp.source_location.join(', '),
         @proc.canonic_container(bp.iseq.source_container).join(' ')]

      vm_loc = "VM offset %d of instruction sequence \"%s\"" % 
        [bp.offset, bp.iseq.name]
      
      loc, other_loc =
        if 'line' == bp.type
          [line_loc, vm_loc]
        else # 'offset' == bp.type
          [vm_loc, line_loc]
        end
      msg(mess + loc + ",\n\t" + other_loc + ".")
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
  cmd.run([name, __LINE__.to_s])
  require 'thread_frame'
  tf = RubyVM::ThreadFrame.current
  pc_offset = tf.pc_offset
  cmd.run([name, "O#{pc_offset}"])
  def foo
    5 
  end
  cmd.run([name, 'foo', (__LINE__-2).to_s])
  cmd.run([name, 'foo'])
end
