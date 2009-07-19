require_relative 'lib/core'
class Debugger
  def initialize
    @core = Core.new
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
