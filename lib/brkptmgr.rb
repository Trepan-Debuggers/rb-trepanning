require_relative 'brkpt'
class BreakpointMgr

  attr_reader :list

  def initialize
    @list = []
  end

  def <<(brkpt)
    @list << brkpt
  end

  def [](index)
    raise TypeError, 
    "index #{index} should be a Fixnum, is #{index.class}" unless
      index.is_a?(Fixnum)
    @list.detect {|bp| bp.id == index }
  end

  alias detect []

  def delete(index)
    bp = detect(index)
    if bp
      bp.unset
      @list = @list.reject{|candidate| candidate == bp}
      return bp
    else
      return nil
    end
  end

  def add(*args)
    brkpt = Breakpoint.new(*args)
    @list << brkpt
    return brkpt
  end

  def empty?
    @list.empty?
  end

  def find(iseq, offset, bind)
    @list.detect do |bp| 
      if bp.enabled? && bp.iseq.equal?(iseq) && bp.offset == offset
        begin
          return bp if bp.condition?(bind)
        rescue
        end 
      end
    end
  end

  def size
    @list.size
  end

  def reset
    @list.each{|bp| bp.unset}
    @list = []
  end

end
if __FILE__ == $0
  iseq = RubyVM::ThreadFrame.current.iseq
  brkpts = BreakpointMgr.new
  brkpts.add(false, 0, iseq)
  p brkpts[2]
  b2 = Breakpoint.new(false, 0, iseq)
  brkpts << b2
  p brkpts.find(b2.iseq, b2.offset)
  p brkpts[2]
  p brkpts.delete(2)
  p brkpts[2]
end
