# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../breakpoint'
require_relative '../../app/breakpoint'
class Trepan::Command::BreakCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
**#{NAME}**
**#{NAME}** *location* [ {if|unless} *condition* ]

Set a breakpoint. In the second form where *condition* is given, the
condition is evaluated in the context of the position. We stop only If
*condition* evalutes to non-false/nil and the "if" form used, or it is
false and the "unless" form used.\

Examples:
---------
    #{NAME}
    #{NAME} 10               # set breakpoint on line 10
    #{NAME} 10 if 1 == a     # like above but only if a is equal to 1
    #{NAME} 10 unless 1 == a # like above but only if a is equal to 1
    #{NAME} me.rb:10
    #{NAME} @20   # set breakpoint VM Instruction Sequence offset 20
    #{NAME} Kernel.pp # Set a breakpoint at the beginning of Kernel.pp

See also:
---------
`condition`, `continue`, `help syntax location`, and `tbreak`
    HELP

    ALIASES      = %w(b)
    CATEGORY     = 'breakpoints'
    SHORT_HELP  = 'Set a breakpoint'
  end

  # This method runs the command
  def run(args, temp=false)
    # FIXME: handle more conditions
    # a line number
    if args.size == 1
      # usage is "break"  which means break right here
      # FIXME: should handle condition
      bp = @proc.breakpoint_offset(@proc.frame.pc_offset,
                                   @proc.frame.iseq, 'true', false)
    else
      iseq, line_number, vm_offset, condition, negate =
        @proc.breakpoint_position(@proc.cmd_argstr, true)
      return false unless iseq && vm_offset
      bp = @proc.breakpoint_offset(vm_offset, iseq, condition, negate, temp)
    end
    if bp
      bp.condition = condition

      if temp
        mess = "Temporary breakpoint %d set at " % bp.id
      else
        mess = "Breakpoint %d set at " % bp.id
      end

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
  tf = RubyVM::Frame.current
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
