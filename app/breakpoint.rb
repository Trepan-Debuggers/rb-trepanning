# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'thread_frame'

# Breakpoint objects
class Trepan
  class Breakpoint
    attr_accessor :condition # If non-nil, this is a String to be eval'd
                             # which must be true to enter the debugger
    attr_accessor :hits      # Fixnum. The number of timea a breakpoint
                             # has been hit (with a true condition). Do
                             # we want to (also) record hits independent
                             # of the condition?
    attr_reader   :id        # Fixnum. Name of breakpoint
    attr_reader   :ignore    # Fixnum. Number of times encountered to ignore
    attr_reader   :iseq      # Instruction sequence associated with this
                             # breakpoint. From this we can derive
                             # information such as source location.
    attr_reader   :offset    # Fixnum. Offset into an instruction
                             # sequence for the location of the
                             # breakpoint
    attr_reader   :negate    # Boolean. Negate sense of condition. Used in 
                             # break if .. and break unless ..
                             # breakpoint
    attr_reader   :type      # String. 'line' if breakpoint requested
                             # at "line" boundary or 'offset'
                             # requested at a specific offset
    @@next_id = 1

    BRKPT_DEFAULT_SETTINGS = {
      :condition => 'true',
      :enabled   => 'true',
      :ignore    =>  0,
      :temp      =>  false,
      :negate    =>  false,
      :type      => 'line',
    } unless defined?(BRKPT_DEFAULT_SETTINGS)
    
    def initialize(iseq, offset, opts = {})
      raise TypeError, 
      "#{iseq} is not an instruction sequence" unless 
        iseq.is_a?(RubyVM::InstructionSequence)
      @iseq = iseq

      raise TypeError, 
      "offset #{offset.inspect} not found in instruction sequence" unless 
        iseq.offset2lines(offset)
      @offset = iseq

      raise TypeError, 
      "type mismatch: #{offset.class} given, Fixnum expected" unless 
        offset.is_a?(Fixnum)
      @offset = offset

      opts = BRKPT_DEFAULT_SETTINGS.merge(opts)
      opts.keys.each do |key|
        self.instance_variable_set('@'+key.to_s, opts[key])
      end

      @hits = 0

      unless @id
        @id = @@next_id 
        @@next_id += 1
      end
      raise RuntimeError,
      "Unable to set breakpoint in #{iseq.name} at offset #{offset}" unless set
    end

    def condition?(bind)
      if @negate != eval(@condition, bind)
        if @ignore > 0
          @ignore -= 1
          return false
        else
          @hits += 1
          return true
        end
      else
        return false
      end
    end

    def disable
      @enabled = false
    end

    def enabled
      @enabled = true
    end

    def enabled=(bool)
      @enabled = bool
    end

    def enabled?
      @enabled
    end

    # Return a one-character "icon" giving the state of the breakpoint
    # 't': temporary breakpoint
    # 'B': enabled breakpoint
    # 'b': disabled breakpoint
    def icon_char
      temp? ? 't' : (enabled? ? 'B' : 'b')
    end

    def set
      @iseq.brkpt_set(@offset)
    end

    def source_container
      @iseq.source_container
    end

    def source_location
      @iseq.offset2lines(@offset)
    end

    def temp?
      @temp
    end

    def remove!
      @iseq.brkpt_unset(@offset)
    end

    # Really shouldn't need this after next_id is removed.
    def self.reset
      @@next_id = 1
    end

  end
end

if __FILE__ == $0
  tf = RubyVM::Frame.current
  iseq = tf.iseq
  b1 = Trepan::Breakpoint.new(iseq, 0)
  p b1
  p b1.source_location
  p b1.source_container
  b2 = Trepan::Breakpoint.new(iseq, 0, :temp => true)
  p b2
  puts "b2 id: #{b2.id}"
  puts "b2 hits: #{b2.hits}"
  puts "b2.condition? #{b2.condition?(tf.binding)}"
  puts "b2 hits: #{b2.hits}"
  begin
    b3 = Breakpoint.new(iseq, iseq.iseq_size)
  rescue TypeError => e
    puts "TypeError (expected): #{e}"
  end
  begin
    b3 = Trepan::Breakpoint.new(5, iseq.iseq_size)
  rescue TypeError => e
    puts "TypeError (expected): #{e}"
  end
  puts Trepan::Breakpoint.class_variable_get('@@next_id')
  Trepan::Breakpoint.reset
  puts Trepan::Breakpoint.class_variable_get('@@next_id')
end
