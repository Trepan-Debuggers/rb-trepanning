# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
# Classes to support gdb-like display/undisplay.

require_relative 'frame'

# return suitable frame signature to key display expressions off of.
def display_signature(frame)
  return nil unless frame
  frame.iseq.object_id
end

# Manage a list of display expressions.
class DisplayMgr
  
  def initialize
    @next = 0
    @list = []
  end
  
  def [](index)
    raise TypeError, 
    "index #{index} should be a Fixnum, is #{index.class}" unless
      index.is_a?(Fixnum)
    @list.detect {|disp| disp.number == index }
  end

  def add(frame, arg, fmt=nil)
    return nil unless frame
    begin
      eval(arg, frame.binding)
    rescue
      return nil
    end
    @next += 1
    d = Display.new(frame, arg, fmt, @next)
    @list << d
    d
  end

  # List all display items; return 0 if none
  def all
    s = []
    unless @list.empty?
      s << "Auto-display expressions now in effect:
Num Enb Expression"
      @list.each do |display|
        s << display.format
      end
    end
    s
  end

  # Delete all display expressions"""
  def clear
    @list = []
  end

  # Delete display expression i
  def delete_index(display_number)
    @list.each_with_index do |display, i|
      if display_number == display.number
        @list[i..i] = []
        return true
      end
    end
    false
  end

  # display any items that are active'''
  def display(frame)
    return unless frame
    s = []
    sig = display_signature(frame)
    @list.each do |display|
      if display.enabled # && display.signature == sig
        s << display.to_s(frame)
      end
    end
    return s
  end

  def enable_disable(display_number, b_enable_disable)
    @list.each do |display|
      if display_number == display.number
        display.enabled = b_enable_disable
        return true
      end
    end
    false
  end

  def max
    @list.map{|disp| disp.number}.max
  end

  def size
    @list.size
  end
end

class Display
  attr_reader   :number
  attr_reader   :signature
  attr_accessor :enabled
  
  def initialize(frame, arg, fmt, number)
    @signature = display_signature(frame)
    @fmt       = fmt
    @arg       = arg
    @enabled   = true
    @number    = number
  end

  def disable
    @enabled = false
  end

  def disabled?
    !@enabled
  end

  def enable
    @enabled = true
  end

  def enabled?
    @enabled
  end

  def to_s(frame)
    return 'No symbol "' + @arg + '" in current context.' unless frame
    
    begin
      val = eval(@arg, frame.binding)
    rescue
      return "No symbol \"#{@arg}\" in current context."
    end
    s = "#{self.format(false)} = #{val}"
    return s
  end

  # format display item
  def format(show_enabled=true)
    what = ''
    what += @enabled ? ' y ' : ' n ' if 
      show_enabled
    what += (@fmt + ' ') if @fmt
    what += @arg if @arg
    '%3d: %s' % [@number, what]
  end

end

if __FILE__ == $0
  # Demo it.
  mgr = DisplayMgr.new

  def print_display(mgr)
    mgr.all.each {|line| puts line}
    puts '=' * 40
  end
    
  require 'thread_frame'
  frame = RubyVM::ThreadFrame::current

  x = 1
  mgr.add(frame, 'x > 1')
  puts "Number of displays %s" % mgr.size
  puts "Max Number  %d" % mgr.max
  print_display(mgr)

  mgr.enable_disable(1, false)
  print_display(mgr)

  mgr.enable_disable(1, true)
  print_display(mgr)

  mgr.clear()
  print_display(mgr)

end
