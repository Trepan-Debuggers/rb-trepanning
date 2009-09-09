#!/usr/bin/env ruby
require 'trace'                      # Trace filtering
require 'thread_frame'
require_relative %w(lib core)        # core event-handling mechanism
require_relative %w(lib default)     # default debugger settings
require_relative %w(interface user)  # user interface (includes I/O)
class Debugger

  attr_accessor :core         # access to Debugger::Core instance
  attr_accessor :intf         # The way the outside world interfaces with us.
  attr_accessor :restart_argv # How to restart us, empty or nil. 
                              # Note restart[0] is typically $0.
  attr_reader   :settings     # Hash[:symbol] of things you can configure
  attr_accessor :trace_filter

  def initialize(settings={})
    @settings     = DbgSettings::DEFAULT_SETTINGS.merge(settings)
    @intf         = Debugger::UserInterface.new
    @core         = Core.new(self, @settings[:core_opts])
    @restart_argv = @settings[:restart_argv]
    @trace_filter = TraceFilter.new
    @trace_filter.excluded << self.method(:debugger)
    @trace_filter.excluded << @core.method(:debugger)
    @trace_filter.excluded << @core.method(:event_processor)
    @trace_filter.excluded << @trace_filter.method(:set_trace_func)
  end

  # Enter the debugger. One-time step you need to do first:
  #    require 'rbdbgr'
  #    mydbg = Debugger.new()
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
  def debugger(opts={}, &block)
    # FIXME: one option we may want to pass is the initial trace filter.
    if block
      @trace_filter.set_trace_func(@core.event_proc)
      block.call(self)
      @trace_filter.set_trace_func(nil)
    elsif opts[:immediate]
      # Stop immediately, but don't show in the call stack the
      # the position of the call we make below, i.e. set the frame
      # one more position farther back.
      @core.debugger(1) 
    else
      # Set to stop on the next event after this returns.
      step_count_save    = @core.step_count
      @core.step_count  = -1 
      @trace_filter.set_trace_func(@core.event_proc)
      Trace.event_masks[0] |= @core.step_events
      @core.step_count = step_count_save
    end
  end
end

if __FILE__ == $0
  def square(x) 
    x * x
  end
  # It is imagined that there are all sorts of command-line options here.
  # (I have a good imagination.)
  dc = Debugger.new(:restart_argv => [File.expand_path($0)])

  puts 'block debugging...'
  dc.debugger {
    a = 2
    b = square(a)
    p "square of #{a} is #{b}"
  }

  puts 'immediate debugging...'
  dc.debugger(:immediate => true)
  puts 'line after immediate'
  a = 3
  square(a)

  class MyClass
    def initialize(x)
      @x = x
    end
  end
  dc.debugger
  m = MyClass.new(5)
  raise RuntimeError
end
