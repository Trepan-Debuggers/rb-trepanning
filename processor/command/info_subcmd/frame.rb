# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative '../../../app/frame'

class Trepan::Subcommand::InfoFrame < Trepan::Subcommand
    include Trepan::Frame
    unless defined?(HELP)
        Trepanning::Subcommand.set_name_prefix(__FILE__, self)
        HELP = <<-EOH
#{CMD}

Show information about the selected frame. The fields we list are:

* A source container and name (e.g. "file" or "string" and the value)
* The actual number of arguments passed in
* The 'arity' or permissible number of arguments passed it. -1 indicates
  variable number
* The frame "type", e.g. TOP, METHOD, BLOCK, EVAL, CFUNC etc.
* The return value if the frame is at a return point
* The PC offset we are currently at; May be omitted of no instruction
  sequence

A backtrace shows roughly the same information in a more compact form.

Example form inside File.basename('foo')

Frame basename
  file  : /tmp/c-func.rb # actually location of caller
  line  : 2  # inherited from caller
  argc  : 1  # One out argument supplied: 'foo'
  arity : -1 # Variable number of args, can have up to 2 arguments.
  type  : CFUNC  (A C function)


See also: backtrace
EOH
        MIN_ABBREV   = 'fr'.size # Note we have "info file"
        MIN_ARGS     = 0
        MAX_ARGS     = 1
        NEED_STACK   = true
        SHORT_HELP   = 'Show information about the selected frame'
    end


    def print_frame_c_params(frame)
        argc = frame.argc
        # FIXME should figure out why exception is raised.
        begin
            if 0 == argc
                return
            elsif frame
                1.upto(argc).map do
                    |i|
                    msg "  \t#{frame.sp(argc-i+3).inspect}"
                end
            else
                msg "  \t??"
            end
        rescue NotImplementedError
            msg "  \t??"
        end
    end

    def run(args)
        if args.size == 2
            frame = @proc.frame
            frame_num = @proc.frame_index
        else
            frame_arg = args[2]
            low, high = @proc.frame_low_high(nil)
            opts={
                :msg_on_error =>
                "The '#{NAME}' command requires a frame number. Got: #{frame_arg}",
                :min_value => low, :max_value => high
            }
            frame_num = @proc.get_an_int(frame_arg, opts)
            frame, frame_num = @proc.get_frame(frame_num, true)
        end
        meth = frame.method rescue nil
        call_info = meth ? format_stack_call(frame, {}) : ''

        section "Frame %2d: %s" % [frame_num, call_info]
        msg "  %-6s: %s" % frame.source_container
        msg "  line  : %s" % @proc.frame_line
        msg "  argc  : %d" % frame.argc
        msg "  arity : %d" % frame.arity
        msg "  type  : %s" % frame.type
        msg "  offset: %d" % frame.pc_offset if frame.iseq
        msg "  label:  %s" % frame.label if frame.label unless meth
        if frame.argc > 0
            msg "  parameters:"
            if meth and frame.type != 'IFUNC'
                iseq = frame.iseq
                if 'CFUNC' == frame.type
                    print_frame_c_params(frame)
                elsif iseq
                    all_param_names(iseq).each do |param|
                        msg "  \t#{param}"
                    end
                end
            end
        end

        if %w(return c_return b_return).member?(@proc.event.to_s)
            @proc.commands['info'].run(%W(info return))
        end
    end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoFrame, false)
  cmd.run(cmd.prefix)
end
