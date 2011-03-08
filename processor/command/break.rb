# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
require_relative '../breakpoint'
require_relative '../../app/breakpoint'
class Trepan::Command::BreakCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [LINE-NUMBER|OFFSET]
#{NAME} METHOD [LINE-NUMBER|OFFSET]

Set a breakpoint. If a line number is given, a breakpoint is set in
that line number of the current instruction sequence. If an offset is
given, a number prefaced with an "@", set a breakpoint at that
instruction offset.

With method name, a breakpoint it set at the beginning of the method.
current instruction sequence. Currently you can add a module name in
front of the method name, like FileUtils.cp, but not a class name like
Dir.pwd. 

Examples:
   #{NAME}
   #{NAME} 10    # set breakpoint on line 10
   #{NAME} @20   # set breakpoint VM Instruction Sequence offset 20
   #{NAME} Kernel.pp # Set a breakpoint at the beginning of Kernel.pp
    HELP

    ALIASES      = %w(b)
    CATEGORY     = 'breakpoints'
    SHORT_HELP  = 'Set a breakpoint'
  end

  # This method runs the command
  def run(args)
    # FIXME: handle more conditions
    # a line number
    if args.size == 1
      # usage is "break"  which means break right here
      # FIXME: should handle condition
      bp = @proc.breakpoint_offset(@proc.frame.pc_offset, 
                                   @proc.frame.iseq) 
    else
      iseq, line_number, vm_offset, condition = 
        @proc.breakpoint_position(@proc.cmd_argstr)
      return false unless iseq && vm_offset
      bp = @proc.breakpoint_offset(vm_offset, iseq)
      # bp = @proc.breakpoint_line(line_number, iseq)
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
  dbgr, cmd = MockDebugger::setup
  # require_relative '../../lib/trepanning'
  def run_cmd(cmd, args) 
    cmd.proc.instance_variable_set('@cmd_argstr', args[1..-1].join(' '))
    cmd.run(args)
  end

  run_cmd(cmd, [cmd.name])
  run_cmd(cmd, [cmd.name, __LINE__.to_s])
  require 'thread_frame'
  tf = RubyVM::ThreadFrame.current
  pc_offset = tf.pc_offset
  run_cmd(cmd, [cmd.name, "@#{pc_offset}"])
  def foo
    5 
  end
  run_cmd(cmd, [cmd.name, 'foo', (__LINE__-2).to_s])
  run_cmd(cmd, [cmd.name, 'foo'])
  run_cmd(cmd, [cmd.name, "MockDebugger::setup"])
  require 'irb'
  run_cmd(cmd, [cmd.name, "IRB.start"])
  run_cmd(cmd, [cmd.name, 'foo93'])
end
