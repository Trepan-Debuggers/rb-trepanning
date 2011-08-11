# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
require_relative '../../app/complete'

class Trepan::Command::WatchgCommand < Trepan::Command
  unless defined?(HELP)
    NAME      = File.basename(__FILE__, '.rb')
    CATEGORY  = 'breakpoints'
    HELP         = <<-EOH
#{NAME} GLOBAL_VARIABLE [on]
#{NAME} GLOBAL_VARIABLE nostop
#{NAME} GLOBAL_VARIABLE off

Use Kernel.trace_var to trace changes of global variable
GLOBAL_VARIABLE.  If nostop is given, then we just print out the 
location and variable name but do not stop in the debugger.

To remove a prior trace, add "off" to the end.

Note in contrast to other events, stopping for variable tracing occurs
*after* the event, not before.

NOTE: this command name will likely change in the future.

Examples:
#{NAME} $PROGRAM_NAME       # enter debugger if global $PROGRAM_NAME changes
#{NAME} $PROGRAM_NAME on    # same as above
#{NAME} $PROGRAN_NAME stop  # just print places the varaible is set
                            # along with the location
#{NAME} $PROGRAN_NAME off   # remove watching changes

See also 'info breakpoints'
    EOH

    MAX_ARGS     = 3
    SHORT_HELP   = "Set to display trace or untrace a global variable."
  end

  def complete(prefix)
    Trepan::Complete.complete_token(global_variables.map{|v| v.to_s},
                                    prefix)
  end

  def trace_var(traced_var, action)
    traced_sym = traced_var.to_sym
    if traced_var[0] == '$'
      unless global_variables.member?(traced_sym)
        msg "Warning: global variable #{traced_var} is not currently defined"
      end
      if @proc.traced_vars.member?(traced_var)
        if @proc.traced_vars[traced_var] == action
          errmsg "global variable #{traced_var} is already traced with #{action}."
          return
        end
      else
        Kernel.trace_var(traced_sym, 
                         lambda {|val| 
                           @proc.core.trace_var_processor(traced_var, val)})
      end
      @proc.traced_vars[traced_var] = action
      msg("Tracing for variable #{traced_var} set to: #{action}.")
    else
      errmsg "Expecting a global variable to trace, got: #{traced_var}"
    end
  end

  def run(args)
    if args.size == 2
      trace_var(args[1], 'stop')
    elsif args.size == 3
      unless %w(nostop on off).member?(args[2])
        errmsg "Expecting third argument to be 'on' 'off' or 'nostop'; got #{args[2]}"
        return
      end
      traced_var = args[1]
      if args[2] == 'off'
        unless @proc.traced_vars.member?(traced_var)
          msg "Warning: variable #{traced_var} is not currently marked as traced."
        end
        untrace_var(traced_var.to_sym)
        @proc.traced_vars.delete(traced_var)
        msg("Removed trace for variable #{traced_var}.")
      else
        traced_var = args[1]
        if @proc.traced_vars.member?(traced_var)
          @proc.traced_vars[traced_var] = args[2]
        else
          trace_var(traced_var, args[2])
        end
      end
    else
      errmsg "Expecting two or three arguments, got #{args.size}"
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  core = dbgr.core
  def core.event_processor(event, frame, arg=nil)
    puts "traced global changed: event: #{event}, frame #{frame}, #{arg.inspect} "
  end
  cmd.run(%w(variable $FOO))
  cmd.run(%w(variable $FOO))
  cmd.run(%w(variable gaga))
  $FOO=0
  cmd.run(%w(variable $FOO off))
  $FOO=1
  cmd.run(%w(variable $FOO off))
end

