# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Nukes output. Used for example in sourcing where you don't want
# to see output.
# 

require_relative 'base_io'

class Trepan
  class OutputNull < Trepan::OutputBase
    def initialize(out, opts={})
      @closed = false
      super
    end

    def close
      @closed = true
    end

    def closed?
      !!@closed
    end

    def flush
    end

    # Use this to set where to write to. output can be a 
    # file object or a string. This code raises IOError on error.
    def write(*args)
    end

    # used to write to a debugger that is connected to this
    # `str' written will have a newline added to it
    #
    def writeline( msg)
    end
  end
end

# Demo it
if __FILE__ == $0
  output = Trepan::OutputNull.new(nil)
  p output
  output.write("Invisible")
  output.writeline("Invisible")
end
