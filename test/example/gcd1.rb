#!/usr/bin/env ruby
require 'thread_frame'
tf = RubyVM::ThreadFrame.current
iseq = tf.iseq
p iseq.child_iseqs
puts iseq.disassemble

# GCD. We assume positive numbers
def gcd(a, b)
  # Make: a <= b
  if a > b
    a, b = [b, a]
  end

  return nil if a <= 0

  if a == 1 or b-a == 0
    return a
  end
  return gcd(b-a, a)
end

a, b = ARGV[0..1].map {|arg| arg.to_i}
puts "The GCD of %d and %d is %d" % [a, b, gcd(a, b)]
