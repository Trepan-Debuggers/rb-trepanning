require_relative 'brkpt'
class BreakpointMgr << Array

  def initialize
    @list = []
  end

  def add(*args)
    @list << Breakpoint.new(*args)
  end

  def <<(brkpt)
    @list << brkpt
  end

end
if __FILE__ == $0
  iseq = RubyVM::ThreadFrame.current.iseq
  bpmgr = BreakpointMgr.new
  bpmgr.add(false, 0, iseq)
  bpmgr << Breakpoint.new(false, 0, iseq)
end
