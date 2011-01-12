# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
# Debugger Socket Input/Output Interface.

require 'socket'
require_relative 'base_io'
require_relative 'tcpfns'

class Trepan 
  # Debugger Client Input/Output Socket.
  class TCPDbgClient < Trepan::InOutBase

    include Trepanning::TCPPacking

    DEFAULT_INIT_OPTS = {:open => true}

    CLIENT_SOCKET_OPTS = {
      :host    => 'localhost', # Symbolic name
      :port    => 1027,  # Arbitrary non-privileged port
    }

    def initialize(opts={})
      @opts      = CLIENT_SOCKET_OPTS.merge(opts)
      @inout     = nil
      @addr      = nil
      @buf       = ''
      @line_edit = false # Our name for GNU readline capability
      @state     = :disconnected
      @inout     = nil
      open(@opts) if @opts[:open]
    end

    # Closes both input and output
    def close
      @state = :closing
      @inout.close if @inout
      @state = :disconnnected
    end

    def open(opts={})
      @opts = CLIENT_SOCKET_OPTS.merge(opts)
      @host = @opts[:host]
      @port = @opts[:port]
      begin
        @inout = TCPSocket.new(@host, @port)
        @state = :connected
      rescue SystemCallError => e
        raise IOError, 
        ('Open client for host %s on port %s gives error: %s' %
         [@host, @port, e])
      end
    end
    
    # Read one message unit. It's possible however that
    # more than one message will be set in a receive, so we will
    # have to buffer that for the next read.
    # EOFError will be raised on EOF.
    def read_msg
      if @state == :connected
        if !@buf || @buf.empty?
          @buf = @inout.recv(TCP_MAX_PACKET)
          if @buf.empty?
            @state = :disconnected
            raise EOFError
          end
        end
        @buf, data = unpack_msg(@buf)
        return data
      else
        raise IOError, ("read_msg called in state: %s." % @state.to_s)
      end
    end

    # This method the debugger uses to write a message unit.
    def write(msg)
      # FIXME: do we have to check the size of msg and split output? 
      @inout.write(pack_msg(msg))
    end

    def writeline(msg)
      write(msg + "\n")
    end
  end
end

# Demo
if __FILE__ == $0
  client = Trepan::TCPDbgClient.new({'open' => false})
  if ARGV.size > 0
    threads = []
    Thread.new do
      server = TCPServer.new('localhost', 1027)
      session = server.accept
      while 'quit' != (line = session.gets)
        session.puts line 
      end
      session.close
    end

    threads << Thread.new do
      print 'Connecting...'
      client.open()
      puts 'connected.'
      while true
        print "input? "
        line = STDIN.gets
        break if line.chomp == 'quit'
        begin
          line = client.writeline(line)
          puts "Got: #{client.read_msg.chomp}"
        rescue EOFError
          puts "Got EOF"
          break
        rescue Exception => e
          puts "Got #{e}"
          break
        end
      end
    end
    threads.each {|t| t.join }
  end
  client.close
end
