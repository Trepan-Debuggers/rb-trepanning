require 'trace'                 # Trace filtering
require 'thread_frame'
require_relative 'lib/core'     # core event-handling mechanism
require_relative 'lib/default'  # default debugger settings
class Debugger

  attr_accessor :core       # access to Debugger::Core instance
  attr_reader   :settings   # Hash[:symbol] of things you can configure

  def initialize(settings={})
    @settings     = DbgSettings::DEFAULT_SETTINGS.merge(settings)
    @core         = Core.new(self, @settings[:core_opts])
    @trace_filter = TraceFilter.new
    @trace_filter.excluded << method(:debugger).to_proc.iseq
  end

  # If you want an synchronous stop in your program call this to
  # enter the debugger command loop.
  # Example:
  #    require 'rbdbgr'
  #    mydbg = Debugger.new()
  #    ... work, work, work
  #    mydbg.debugger(:immediate=>true)   # enter debugger here
  #    ... work, work, work
  #
  # If you want to debug just a block:
  #   require 'rbdbgr'
  #   mydbg = Debugger.new()
  #   mydbg.debugger {
  #     ... code you want to debug.
  #   }

  def debugger(opts={}, &block)
    # FIXME: one option we may want to pass is the initial trace filter.
    if block
      p @core.method(:event_processor)
      @trace_filter.set_trace_func(@core.method(:event_processor).to_proc)
      block.call(self)
      @trace_filter.set_trace_func(nil)
    elsif opts[:immediate]
      @core.debugger
    end
  end
end

if __FILE__ == $0
  def square(x) 
    x * x
  end
  # It is imagined that there are all sorts of command-line options here.
  # (I have a good imagination.)
  dc = Debugger.new()
  puts 'block debugging...'
  dc.debugger {
    a = 2
    b = square(a)
    p "square of #{a} is #{b}"
  }
  # puts 'immediate debugging...'
  # dc.debugger(:immediate => true)
end
