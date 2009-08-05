require_relative 'lib/core'     # core event-handling mechanism
require_relative 'lib/default'  # default debugger settings
class Debugger

  attr_accessor :core       # access to Debugger::Core instance
  attr_reader   :settings   # Hash[:symbol] of things you can configure

  def initialize(settings={})
    @settings = DbgSettings::DEFAULT_SETTINGS.merge(settings)
    @core = Core.new(self, @settings[:core_opts])
  end

  # If you want an synchronous stop in your program call this to
  # enter the debugger command loop.
  # Example:
  #    require 'rbdbgr'
  #    mydbg = Debugger.new()
  #    ... work, work, work
  #    mydbg.debugger   # enter debugger here
  #    ... work, work, work
  def debugger
    @core.debugger
  end
end

if __FILE__ == $0
  # It is imagined that there are all sorts of command-line options here.
  # (I have a good imagination.)
  dc = Debugger.new()
  dc.debugger()
end
