# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'set'
require_relative 'breakpoint'
class Trepan
  class BreakpointMgr

    attr_reader :list
    attr_reader :set

    def initialize
      clear
    end

    def clear
      @list = []
      @next_id = 1
      @set = Set.new
    end

    # Remove all breakpoints that we have recorded
    def finalize
      @list.each do |bp|
        bp.remove!
      end
      clear
    end

    def <<(brkpt)
      @list << brkpt
      @set.add(set_key(brkpt))
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

    def delete_by_brkpt(delete_bp)
      @list = @list.reject{|candidate| candidate == delete_bp}
      @set  = Set.new(@list.map{|bp| set_key(bp)})
      delete_bp.remove! unless @set.member?(set_key(delete_bp))
      return delete_bp
    end

    def add(*args)
      if args[2] 
        unless args[2].member?(:id)
          args[2][:id] = @next_id
          @next_id += 1
        end
      else
        args[2] = {:id => @next_id}
        @next_id += 1
      end

      brkpt = Trepan::Breakpoint.new(*args)
      @list << brkpt
      @set.add(set_key(brkpt))
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

    def max
      @list.map{|bp| bp.id}.max
    end

    # Key used in @set to list unique instruction-sequence offsets.
    def set_key(bp)
      [bp.iseq, bp.offset]
    end

    def size
      @list.size
    end

    def reset
      @list.each{|bp| bp.remove!}
      @list = []
      @set  = Set.new
    end
  end
end
if __FILE__ == $0
  def bp_status(brkpts, i)
    puts "list size: #{brkpts.list.size}"
    puts "set size: #{brkpts.set.size}"
    puts "max: #{brkpts.max}"
    p brkpts
    puts "--- #{i} ---"
  end

  frame = RubyVM::ThreadFrame.current 
  iseq = frame.iseq
  brkpts = Trepan::BreakpointMgr.new
  brkpts.add(iseq, 0)
  p brkpts[2]
  bp_status(brkpts, 1)
  offset = frame.pc_offset
  b2 = Trepan::Breakpoint.new(iseq, offset)
  brkpts << b2
  p brkpts.find(b2.iseq, b2.offset, nil)
  p brkpts[2]
  puts '--- 2 ---'
  p brkpts.line_breaks(iseq.source_container)
  p brkpts.delete(2)
  p brkpts[2]
  bp_status(brkpts, 3)

  # Two of the same breakpoints but delete 1 and see that the
  # other still stays
  offset = frame.pc_offset
  b2 = Trepan::Breakpoint.new(iseq, offset)
  brkpts << b2
  bp_status(brkpts, 4)
  b3 = Trepan::Breakpoint.new(iseq, offset)
  brkpts << b3
  bp_status(brkpts, 5)
  brkpts.delete_by_brkpt(b2)
  bp_status(brkpts, 6)
  brkpts.delete_by_brkpt(b3)
  bp_status(brkpts, 7)
end
