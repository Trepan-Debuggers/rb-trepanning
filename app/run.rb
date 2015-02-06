# -*- coding: utf-8 -*-
# Copyright (C) 2010-2013, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require 'rbconfig'
module Trepanning

    class Termination < RuntimeError
    end

    module_function # All functions below are easily publically accessible

    def run_program(dbgr, program_to_debug)
        RubyVM::Frame::get.trace_off1 = true
        RubyVM::Frame::get.trace_off =  false
        dbgr.core.processor.hidelevels[Thread.current] =
            RubyVM::Frame.stack_size
        dbgr.trace_point.enable

        # FIXME: the magic skip count 4 below is to skip over
        # the following calls triggered by Kernel::load
        #   c_call   - IO#set_encoding(1)
        #   c_return - IO#set_encoding -> *debugged program*
        #   call     - IO#set_encoding(1)
        #   c_return - IO#set_encoding -> *debugged program*
        # This is  not very robust. Figure out how to
        # address this.
        dbgr.core.step_count = 4
        Kernel::load program_to_debug
    end


  # Given a Ruby interpreter and program we are to debug, debug it.
  # The caller must ensure that ARGV is set up to remove any debugger
  # arguments or things that the debugged program isn't supposed to
  # see.  FIXME: Should we make ARGV an explicit parameter?
  def debug_program(dbgr, program_to_debug)

      # Make sure Ruby script syntax checks okay.
      # Otherwise we get a load message that looks like trepanning has
      # a problem.
      output = ruby_syntax_errors(program_to_debug)
      if output
          puts output
          exit $?.exitstatus
      end

      # dbgr.trace_filter << self.method(:debug_program)
      # dbgr.trace_filter << Kernel.method(:load)
      dbgr.trace_filter << 'debug_program'
      dbgr.trace_filter << 'load'

      old_dollar_0 = $0

      # Without the dance below to set $0, setting it to a signifcantly
      # longer value will truncate it in some OS's. See
      # http://www.ruby-forum.com/topic/187083
      $progname = program_to_debug
      alias $0 $progname
      dollar_0_tracker = lambda {|val| $program_name = val}
      trace_var(:$0, dollar_0_tracker)

      begin
          dbgr.start(false)
          frame = RubyVM::Frame.get
          while frame do
              frame.trace_off = true
              frame = frame.prev
          end
          run_program(dbgr, program_to_debug)
          raise Termination
      rescue Termination
          if dbgr.settings[:cmdloop_on_exit]
              dbgr.stop
              puts "Program terminated, type q to quit"
              dbgr.core.processor.process_commands(nil, 0)
          end
      rescue Interrupt
      end

      # The dance we have to undo to restore $0 and undo the mess created
      # above.
      $0 = old_dollar_0
      untrace_var(:$0, dollar_0_tracker)
  rescue
      if dbgr.settings[:post_mortem]
          frame = RubyVM::Frame.get
          dbgr.core.step_count = 0  # Make event processor stop
          dbgr.core.processor.settings[:debugstack] = 0  # Make event processor stop
          dbgr.core.event_processor('post-mortem', frame, $!)
      else
      raise
      end
  end

  # Do a shell-like path lookup for prog_script and return the results.
  # If we can't find anything return prog_script.
  def whence_file(prog_script)
    if RbConfig::CONFIG['target_os'].start_with?('mingw')
      if (prog_script =~ /^[a-zA-Z][:]/)
        start = prog_script[2..2]
        if [File::ALT_SEPARATOR, File::SEPARATOR].member?(start)
          return prog_script
        end
      end
    end
    if prog_script.start_with?(File::SEPARATOR) || prog_script.start_with?('.')
      # Don't search since this name has path is explicitly absolute or
      # relative.
      return prog_script
    end
    for dirname in ENV['PATH'].split(File::PATH_SEPARATOR) do
      prog_script_try = File.join(dirname, prog_script)
      return prog_script_try if File.readable?(prog_script_try)
    end
    # Failure
    return prog_script
  end

  def ruby_syntax_errors(prog_script)
    output = `#{RbConfig.ruby} -c #{prog_script.inspect} 2>&1`
    if $?.exitstatus != 0 and RUBY_PLATFORM !~ /mswin/
      return output
    end
    return nil
  end
  module_function :ruby_syntax_errors
end

if __FILE__ == $0
  # Demo it.
  include  Trepanning
  puts whence_file('irb')
  puts whence_file('probably-does-not-exist')
  puts RbConfig.ruby
  puts "#{__FILE__} is syntactically correct" unless
    ruby_syntax_errors(__FILE__)
  readme = File.join(File.dirname(__FILE__), '..', 'README.textile')
  puts "#{readme} is not syntactically correct" if
    ruby_syntax_errors(readme)
end
