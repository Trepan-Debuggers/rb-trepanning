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
      delete_by_brkpt(bp)
      return bp
    else
      return nil
    end
  end

  def delete_by_brkpt(bp)
    bp.unset
    @list = @list.reject{|candidate| candidate == bp}
    return bp
  end

  def add(*args)
    brkpt = Breakpoint.new(*args)
    @list << brkpt
    return brkpt
  end

  def empty?
    @list.empty?
  end

  def line_breaks(container)
    result = {}
    @list.each do |bp|
      if bp.source_container == container
        bp.source_location.each do |line|
          result[line] = bp 
        end
      end
    end
    result
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
  p brkpts.find(b2.iseq, b2.offset, nil)
  p brkpts[2]
  p '---'
  p brkpts.line_breaks(iseq.source_container)
  p '---'
  p brkpts.delete(2)
  p brkpts[2]
end
