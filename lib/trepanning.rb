#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'trace'                          # Trace filtering
require 'thread_frame'                   # Stack frame introspection and more.
require_relative '../app/complete'       # command completion
require_relative '../app/core'           # core event-handling mechanism
require_relative '../app/default'        # default debugger settings
require_relative '../interface/user'     # user interface (includes I/O)
require_relative '../interface/script'   # --command interface (includes I/O)
require_relative '../interface/client'   # client interface (remote debugging)
require_relative '../interface/server'   # server interface (remote debugging)

# SCRIPT_ISEQS__ is like SCRIPT_LINES__ in a patched Ruby 1.9. Setting
# this variable to a hash causes instruction sequences to be added in
# this has under their "filename" as a key. More accurately though,
# the "filename" is instruction sequence name that was given as in the
# "filename" parameter when the instruction sequence was
# generated. Each value is an array of instruction sequences that
# share that name.
SCRIPT_ISEQS__ = {} unless 
  defined?(SCRIPT_ISEQS__) && SCRIPT_ISEQS__.is_a?(Hash)
ISEQS__        = {} unless 
  defined?(ISEQS__) && ISEQS__.is_a?(Hash)

class Trepan

  attr_reader   :completion_proc # GNU Readline completion proc
  attr_accessor :core         # access to Trepan::Core instance
  attr_accessor :intf         # Array. The way the outside world
                              # interfaces with us.  An array, so that
                              # interfaces can be stacked.
  attr_accessor :restart_argv # How to restart us, empty or nil.
                              # Note: restart_argv is typically C's
                              # **argv, not Ruby's ARGV. So
                              # restart_argv[0] is $0.
  attr_reader   :settings     # Hash[:symbol] of things you can configure
  attr_accessor :trace_filter # Procs/Methods we ignore.

  def initialize(settings={})

    # FIXME: Tracing through intialization code is slow. Need to figure
    # out better ways to do this. 
    th = Thread.current
    th.exec_event_tracing  = true

    @settings = Trepan::DEFAULT_SETTINGS.merge(settings)
    @input  = @settings[:input] || STDIN
    @output = @settings[:output] || STDOUT

    @completion_proc = method(:completion_method)

    @intf = 
      if @settings[:server]
        @completion_proc = nil
        opts = Trepan::ServerInterface::DEFAULT_INIT_CONNECTION_OPTS.dup
        opts[:port] = @settings[:port] if @settings[:port]
        opts[:host] = @settings[:host] if @settings[:host]
        opts[:readline] = false
        puts("starting debugger in out-of-process mode port at " +
             "#{opts[:host]}:#{opts[:port]}")
        [Trepan::ServerInterface.new(nil, nil, opts)]
      elsif @settings[:client]
        opts = Trepan::ClientInterface::DEFAULT_INIT_CONNECTION_OPTS.dup
        opts[:port] = @settings[:port] if @settings[:port]
        opts[:host] = @settings[:host] if @settings[:host]
        opts[:complete] = @completion_proc
        opts[:readline] ||= @settings[:readline]
        [Trepan::ClientInterface.new(nil, nil, nil, nil, opts)]
      else
        opts = {:complete => @completion_proc,
                :readline => @settings[:readline]}
        [Trepan::UserInterface.new(@input, @output, opts)]
      end

    process_cmdfile_setting(@settings)

    # FIXME: The below option settings is a big crock.
    @settings[:core_opts][:cmdproc_opts] ||= {}
    @settings[:core_opts][:cmdproc_opts][:highlight] ||= settings[:highlight]

    @core = Core.new(self, @settings[:core_opts])
    
    if @settings[:initial_dir]
      Dir.chdir(@settings[:initial_dir])
    else
      @settings[:initial_dir] = Dir.pwd
    end
    @initial_dir  = @settings[:initial_dir]
    @restart_argv = @settings[:restart_argv]

    unless @settings[:client]
      @trace_filter = Trace::Filter.new
      %w(debugger start stop).each do |m| 
        @trace_filter << self.method(m.to_sym)
      end
      %w(debugger event_processor trace_var_processor).each do 
        |m| 
        @trace_filter << @core.method(m)
      end
      @trace_filter << @trace_filter.method(:add_trace_func)
      @trace_filter << @trace_filter.method(:remove_trace_func)
      @trace_filter << Kernel.method(:add_trace_func)
    end

    # Run user debugger command startup files.
    add_startup_files unless @settings[:nx]
    add_command_file(@settings[:restore_profile]) if 
      @settings[:restore_profile] && File.readable?(@settings[:restore_profile])

    at_exit do 
      clear_trace_func
      @intf[-1].close 
    end
    th.exec_event_tracing  = false
  end

  # The method is called when we want to do debugger command completion
  # such as called from GNU Readline with <TAB>.
  def completion_method(last_token, leading=Readline.line_buffer)
    completion = @core.processor.complete(leading, last_token)
    if 1 == completion.size 
      completion_token = completion[0]
      if last_token.end_with?(' ')
        if last_token.rstrip == completion_token 
          # There is nothing more to complete
          []
        else
          []
        end
      else
        [completion_token]
      end
    else
      # We have multiple completions. Get the last token so that will
      # be presented as a list of completions.
      completion
    end
  end

  # To call from inside a Ruby program, there is one-time setup that 
  # needs to be done first:
  #    require 'trepanning'
  #    mydbg = Trepan.new()
  # which will tell the debugger how to "restart" the program.
  #
  # If you want a synchronous stop in your program call to the
  # debugger at the point of the call, set opts[:immediate]
  # true. Example:
  #
  #    ... work, work, work
  #    mydbg.debugger(:immediate=>true)   # enter debugger here
  #    ... work, work, work
  #
  # However to enter the debugger on the next event after the 
  # debugger() call:
  #  
  #    ... work, work, work
  #    mydbg.debugger  # Don't stop here...
  #    work            # but stop here.
  #
  # And finally, if you want to debug just a block:
  #   mydbg.debugger {
  #     ... code you want to debug.
  #   }
  #
  # Some options

  #   :immediate -  boolean. If true, immediate stop rather than wait
  #                          for an event
  #
  #   :hide_stack - boolean. If true, omit stack frames before the
  #                          debugger call
  # 
  #   :debugme    - boolean. Allow tracing into this routine. You
  #                          generally won't want this. It slows things
  #                          down horribly.

  def debugger(opts={}, &block)
    # FIXME: one option we may want to pass is the initial trace filter.
    if opts[:hide_stack]
      @core.processor.hidelevels[Thread.current] = 
        RubyVM::ThreadFrame.current.stack_size
    end
    th = Thread.current
    if block
      start
      ret = block.call
      stop
      return ret
    elsif opts[:immediate]
      # Stop immediately after this method returns. But if opts[:debugme]
      # is set, we can stop in this method.
      RubyVM::ThreadFrame::current.trace_off = true unless opts[:debugme]
      @trace_filter.set_trace_func(@core.event_proc) 
      Trace.event_masks[0] |= @core.step_events
      @core.debugger(1) 
    else
      RubyVM::ThreadFrame::current.trace_off = true unless opts[:debugme]

      @trace_filter.set_trace_func(@core.event_proc)
      Trace.event_masks[0] |= @core.step_events

      # Set to stop on the next event after this returns.
      @core.step_count = opts[:step_count] || 0
    end
  end

  # Set core's trace-event processor to run
  def start
    @trace_filter.add_trace_func(@core.event_proc)
  end
  
  # Remove all of our trace events
  def stop(opts={})
    # FIXME: should do something in the middle when
    # we have the ability to remove *our* specific hook
    # @trace_filter.set_trace_func(nil)
    # @trace_filter.remove_trace_func
    clear_trace_func
  end

  def add_command_file(cmdfile, stderr=$stderr)
    unless File.readable?(cmdfile)
      if File.exists?(cmdfile)
        stderr.puts "Command file '#{cmdfile}' is not readable."
        return
      else
        stderr.puts "Command file '#{cmdfile}' does not exist."
        return
      end
    end
    @intf << Trepan::ScriptInterface.new(cmdfile)
  end

  def add_startup_files()
    seen = {}
    cwd_initfile = File.join('.', Trepan::CMD_INITFILE_BASE)
    [cwd_initfile, Trepan::CMD_INITFILE].each do |initfile|
      full_initfile_path = File.expand_path(initfile)
      next if seen[full_initfile_path]
      add_command_file(full_initfile_path) if File.readable?(full_initfile_path)
      seen[full_initfile_path] = true
    end
  end

  def process_cmdfile_setting(settings)
    settings[:cmdfiles].each do |item|
      cmdfile, opts = 
        if item.kind_of?(Array)
          item
        else
          [item, {}]
        end
      add_command_file(cmdfile, opts)
    end if settings.member?(:cmdfiles)
  end

  # As a simplification for creating a debugger object, and then
  # calling using the object to invoke the debugger, we allow this
  # two-step process in one step. That is, instead of
  #  
  #  require 'trepanning'
  #  mydbg = Trepan.new()
  #  ... 
  #  mydbg.debugger

  # You can run:
  #  require 'trepanning'
  # ...
  #  Trepan.debug
  #
  # See debugger for options that can be passed. By default :hide_stack is
  # set.
  # 
  # Likewise for mydbg.debugger{ ... }

  def self.debug(opts={}, &block)
    opts = {:hide_stack => false}.merge(opts)
    unless defined?($trepanning) && $trepanning.is_a?(Trepan)
      $trepanning = Trepan.new(opts)
      $trepanning.trace_filter << self.method(:debug)
    end
    $trepanning.debugger(opts, &block)
  end

  def self.debug_str(string, opts = DEFAULT_DEBUG_STR_SETTINGS)
    $trepanning = Trepan.new(opts) unless 
      $trepanning && $trepanning.is_a?(Trepan)
    $trepanning.core.processor.settings[:different] = false
    # Perhaps we should do a remap file to string right here? 
    $trepanning.debugger(opts) { eval(string) }
  end
end

module Kernel
  # Same as Trepan.debug. 
  # FIXME figure out a way to remove duplication.
  def debugger(opts={}, &block)
    opts = {:hide_stack => false}.merge(opts)
    unless defined?($trepanning) && $trepanning.is_a?(Trepan)
      $trepanning = Trepan.new(opts)
      $trepanning.trace_filter << self.method(:debugger)
    end
    $trepanning.debugger(opts)
  end
end

if __FILE__ == $0
  def square(x) # :nodoc
    x * x
  end
  puts 'block debugging...'
  # It is imagined that there are all sorts of command-line options here.
  # (I have a good imagination.)
  Trepan.debug {
    a = 2
    b = square(a)
    p "square of #{a} is #{b}"
  }

  puts 'immediate debugging...'
  $trepanning.debugger(:immediate => true)
  puts 'line after immediate'
  a = 3
  square(a)

  class MyClass
    def initialize(x)
      @x = x
    end
  end
  $trepanning.debugger
  m = MyClass.new(5)
  raise RuntimeError # To see how we handle post-mortem debugging.
end
