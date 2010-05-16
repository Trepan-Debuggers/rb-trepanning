#!/usr/bin/env ruby
require 'trace'                          # Trace filtering
require 'thread_frame'
require_relative '../app/core'           # core event-handling mechanism
require_relative '../app/default'        # default debugger settings
require_relative '../interface/user'     # user interface (includes I/O)
require_relative '../interface/script'   # --command interface (includes I/O)

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

class Debugger

  attr_accessor :core         # access to Debugger::Core instance
  attr_accessor :intf         # Array. The way the outside world
                              # interfaces with us.  An array, so that
                              # interfaces can be stacked.
  attr_reader   :initial_dir  # String. Current directory when program
                              # started. Used in restart program.
  attr_accessor :restart_argv # How to restart us, empty or nil. 
                              # Note restart[0] is typically $0.
  attr_reader   :settings     # Hash[:symbol] of things you can configure
  attr_accessor :trace_filter # Procs/Methods we ignore.

  def initialize(settings={})

    # FIXME: Tracing through intialization code is slow. Need to figure
    # out better ways to do this. 
    th = Thread.current
    th.exec_event_tracing  = true

    @settings = Rbdbgr::DEFAULT_SETTINGS.merge(settings)
    @input  ||= @settings[:input]
    @output ||= @settings[:output]

    @intf     = [Debugger::UserInterface.new(@input, @output)]
    @settings[:cmdfiles].each do |cmdfile|
      add_command_file(cmdfile)
    end if @settings.member?(:cmdfiles)
    @core     = Core.new(self, @settings[:core_opts])
    if @settings[:initial_dir]
      Dir.chdir(@settings[:initial_dir])
    else
      @settings[:initial_dir] = Dir.pwd
    end
    @initial_dir  = @settings[:initial_dir]
    @restart_argv = 
      if @settings[:set_restart]
        [File.expand_path($0)] + ARGV
      elsif @settings[:restart_argv]
        @settings[:restart_argv]
      else 
        nil
      end
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

  # To call from inside a Ruby program, there is one-time setup that 
  # needs to be done first:
  #    require 'rbdbgr'
  #    mydbg = Debugger.new()
  # or if you haven't mucked around with $0 and ARGV, you might try:
  #    mydbg = Debugger.new(:set_restart=>true))
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

  #   :immediate -  boolean. If true, mmediate stop rather than wait
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
      # I don't think yield or block.call is quite right.
      ret = yield   # Not: block.call(self) ? 
      stop
      return ret
    elsif opts[:immediate]
      # Stop immediately, but don't show in the call stack the
      # the position of the call we make below, i.e. set the frame
      # one more position farther back.
      # FIXME: do better saving/restoring event_exec_tracing.
      unless opts[:debugme]
        old_exec_event_tracing = th.exec_event_tracing
        th.exec_event_tracing  = true 
      end
      @trace_filter.set_trace_func(@core.event_proc) 
      Trace.event_masks[0] |= @core.step_events
      th.exec_event_tracing  = old_exec_event_tracing unless opts[:debugme]
      @core.debugger(1) 
    else
      # FIXME: do better saving/restoring event_exec_tracing.
      unless opts[:debugme]
        old_exec_event_tracing = th.exec_event_tracing
        th.exec_event_tracing  = true 
      end

      @trace_filter.set_trace_func(@core.event_proc)
      Trace.event_masks[0] |= @core.step_events
      th.exec_event_tracing  = old_exec_event_tracing unless opts[:debugme]

      # Set to stop on the next event after this returns.
      @core.step_count = 0
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
        stderr.puts caller
        return
      end
    end
    @intf << Debugger::ScriptInterface.new(cmdfile, @output)
  end

  def add_startup_files()
    seen = {}
    cwd_initfile = File.join('.', CMD_INITFILE_BASE)
    [cwd_initfile, CMD_INITFILE].each do |initfile|
      full_initfile_path = File.expand_path(initfile)
      next if seen[full_initfile_path]
      add_command_file(full_initfile_path) if File.readable?(full_initfile_path)
      seen[full_initfile_path] = true
    end
  end

  # As a simplification for creating a debugger object, and then
  # calling using the object to invoke the debugger, we allow this
  # two-step process in one step. That is, instead of
  #  
  #  require 'rbdbgr'
  #  mydbg = Debugger.new()
  #  ... 
  #  mydbg.debugger

  # You can run:
  #  require 'rbdbgr'
  # ...
  #  Debugger.debug
  #
  # See debugger for options that can be passed. By default :hide_stack is
  # set.
  # 
  # Likewise for mydbg.debugger{ ... }

  def self.debug(opts={}, &block)
    opts = {:hide_stack => true}.merge(opts)
    unless defined?($rbdbgr) && $rbdbgr.is_a?(Debugger)
      $rbdbgr = Debugger.new(opts)
      $rbdbgr.trace_filter << self.method(:debug)
    end
    $rbdbgr.debugger(opts, &block)
  end

  def self.debug_str(string, opts = DEFAULT_DEBUG_STR_SETTINGS)
    $rbdbgr = Debugger.new(opts) unless $rbdbgr && $rbdbgr.is_a?(Debugger)
    $rbdbgr.core.processor.settings[:different] = false
    # Perhaps we should do a remap file to string right here? 
    $rbdbgr.debugger(opts) { eval(string) }
  end
end

module Kernel
  # Same as Debugger.debug. 
  # FIXME figure out a way to remove duplication.
  def rbdbgr(opts={}, &block)
    opts = {:hide_stack => true}.merge(opts)
    unless defined?($rbdbgr) && $rbdbgr.is_a?(Debugger)
      $rbdbgr = Debugger.new
      $rbdbgr.trace_filter << self.method(:rbdbgr)
    end
    $rbdbgr.debugger(opts, &block)
  end
end

if __FILE__ == $0
  def square(x) 
    x * x
  end
  puts 'block debugging...'
  # It is imagined that there are all sorts of command-line options here.
  # (I have a good imagination.)
  Debugger.debug(:set_restart=>true) {
    a = 2
    b = square(a)
    p "square of #{a} is #{b}"
  }

  puts 'immediate debugging...'
  $rbdbgr.debugger(:immediate => true)
  puts 'line after immediate'
  a = 3
  square(a)

  class MyClass
    def initialize(x)
      @x = x
    end
  end
  $rbdbgr.debugger
  m = MyClass.new(5)
  raise RuntimeError # To see how we handle post-mortem debugging.
end
