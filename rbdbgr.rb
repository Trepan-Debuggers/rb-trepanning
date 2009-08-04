require_relative 'lib/core'     # core event-handling mechanism
require_relative 'lib/default'  # default debugger settings
class Debugger
  attr_accessor :core
  attr_accessor :settings   # Hash of things you can configure
  def initialize(settings={})
    @settings = DbgSettings::DEFAULT_SETTINGS.merge(settings)
    @core = Core.new(self, @settings[:core_settings] || {})
  end
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
