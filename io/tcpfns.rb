# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
# Subsidiary routines used to "pack" and "unpack" TCP messages.

module Trepanning
  module TCPPacking

    unless defined?(TCP_MAX_PACKET)
      TCP_MAX_PACKET = 8192 # Largest size for a recv
      LOG_MAX_MSG    = Math.log10(TCP_MAX_PACKET).ceil
    end
      
    def pack_msg(msg)
      fmt = '%%%dd' % LOG_MAX_MSG # A funny way of writing: '%4d'
      (fmt % msg.size) + msg
    end

    def unpack_msg(buf)
      length  = Integer(buf[0...LOG_MAX_MSG])
      data    = buf[LOG_MAX_MSG..LOG_MAX_MSG+length]
      buf     = buf[LOG_MAX_MSG+length..-1]
      [buf, data]
    end
  end
end

# Demo
if __FILE__ == $0
  include Trepanning::TCPPacking
  msg = "Hi there!"
  puts unpack_msg(pack_msg(msg))[1] == msg
end

