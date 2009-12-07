# -*- coding: utf-8 -*-
require 'thread_frame'

# Breakpoint objects
class Breakpoint
  attr_accessor :condition # If non-nil, this is a String to be eval'd
                           # which must be true to enter the debugger
  attr_accessor :hits      # Fixnum. The number of timea a breakpoint
                           # has been hit (with a true condition). Do
                           # we want to (also) record hits independent
                           # of the condition?
  attr_reader   :id        # Fixnum. Name of breakpoint
  attr_reader   :ignore    # Fixnum. Number of times encounterd to ignore
  attr_reader   :iseq      # Instruction sequence associated with this
                           # breakpoint. From this we can derive
                           # information such as source location.
  attr_reader   :offset    # Fixnum. Offset into an instruction
                           # sequence for the location of the
                           # breakpont
  @@next_id = 1

  def initialize(is_temporary, offset, iseq, condition = 'true',
                 enabled = 'true')
    @condition = condition
    @enabled   = enabled
    @hits      = 0
    @id        = @@next_id
    @ignore    = 0

    raise TypeError, 
    "#{iseq} is not an instruction sequence" unless 
      iseq.is_a?(RubyVM::InstructionSequence)
    
    @iseq      = iseq
    raise TypeError, 
    "offset #{offset.inspect} not found in instruction sequence" unless 
      iseq.offset2lines(offset)
    @offset    = offset 
    @@next_id += 1
    @temp      = is_temporary
    set
  end

  def condition?(bind)
    if eval(@condition, bind)
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

  def unset
    @iseq.brkpt_unset(@offset)
  end

end

if __FILE__ == $0
  tf = RubyVM::ThreadFrame.current
  iseq = tf.iseq
  b1 = Breakpoint.new(false, 0, iseq)
  p b1
  p b1.source_location
  p b1.source_container
  b2 = Breakpoint.new(true, 0, iseq)
  p b2
  puts "b2 id: #{b2.id}"
  puts "b2 hits: #{b2.hits}"
  puts "b2.condition? #{b2.condition?(tf.binding)}"
  puts "b2 hits: #{b2.hits}"
  begin
    b3 = Breakpoint.new(true, iseq.iseq_size, 5)
  rescue TypeError => e
    puts "TypeError (expected): #{e}"
  end
  begin
    b3 = Breakpoint.new(true, iseq.iseq_size, iseq)
  rescue TypeError => e
    puts "TypeError (expected): #{e}"
  end
end
