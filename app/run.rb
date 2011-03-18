# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require 'rbconfig'
module Trepanning

  module_function # All functions below are easily publically accessible

  # Given a Ruby interpreter and program we are to debug, debug it.
  # The caller must ensure that ARGV is set up to remove any debugger
  # arguments or things that the debugged program isn't supposed to
  # see.  FIXME: Should we make ARGV an explicit parameter?
  def debug_program(dbgr, ruby_path, program_to_debug, start_opts={})

    # Make sure Ruby script syntax checks okay.
    # Otherwise we get a load message that looks like trepanning has 
    # a problem. 
    output = `#{RbConfig.ruby} -c #{program_to_debug.inspect} 2>&1`
    if $?.exitstatus != 0 and RUBY_PLATFORM !~ /mswin/
      puts output
      exit $?.exitstatus 
    end
    # print "\032\032starting\n" if Trepan.annotate and Trepan.annotate > 2

    dbgr.trace_filter << self.method(:debug_program)
    dbgr.trace_filter << Kernel.method(:load)

    old_dollar_0 = $0

    # Without the dance below to set $0, setting it to a signifcantly
    # longer value will truncate it in some OS's. See
    # http://www.ruby-forum.com/topic/187083
    $progname = program_to_debug
    alias $0 $progname
    dollar_0_tracker = lambda {|val| $program_name = val} 
    trace_var(:$0, dollar_0_tracker)

    dbgr.debugger(:hide_stack=>true) do
      dbgr.core.processor.hidelevels[Thread.current] = 
        RubyVM::ThreadFrame.current.stack_size + 1
      begin
        Kernel::load program_to_debug
      rescue Interrupt
      end
    end

    # The dance we have to undo to restore $0 and undo the mess created
    # above.
    $0 = old_dollar_0
    untrace_var(:$0, dollar_0_tracker)
  end

  # Do a shell-like path lookup for prog_script and return the results.
  # If we can't find anything return prog_script.
  def whence_file(prog_script)
    if prog_script.index(File::SEPARATOR)
      # Don't search since this name has path separator components
      return prog_script
    end
    for dirname in ENV['PATH'].split(File::PATH_SEPARATOR) do
      prog_script_try = File.join(dirname, prog_script)
      return prog_script_try if File.readable?(prog_script_try)
    end
    # Failure
    return prog_script
  end
end

if __FILE__ == $0
  # Demo it.
  include  Trepanning
  puts whence_file('irb')
  puts whence_file('probably-does-not-exist')
end
