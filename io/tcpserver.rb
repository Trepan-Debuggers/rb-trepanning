# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
# Debugger Server Input/Output interface.

require 'socket'
require_relative '../app/default' # For host and port
require_relative 'base_io'
require_relative 'tcpfns'

class Trepan 
  # Debugger Server Input/Output Socket.
  class TCPDbgServer < Trepan::InOutBase

    include Trepanning::TCPPacking
    
    DEFAULT_INIT_OPTS = {:open => true}
    
    SERVER_SOCKET_OPTS = {
      :host    => Trepan::DEFAULT_SETTINGS[:host],
      :port    => Trepan::DEFAULT_SETTINGS[:port], # A non-privileged port
      :timeout => 5,     # FIXME: not used
      :reuse   => true,  # FIXME: not used. Allow port to be resued on close?
                         # Python has: 'posix' == os.name 
    }

    attr_reader :state

    def initialize(opts={})
      @opts    = DEFAULT_INIT_OPTS.merge(opts)
      @input = @output = @session = nil
      @buf     = ''    # Read buffer
      @state   = :disconnected
      @port    = nil   # Current port in use
      @host    = nil   # current host in use
      @line_edit = false
      open(@opts) if @opts[:open]
    end

    def connected?
      :connected == @state
    end
    

    # Closes server connection.
    def close
      @state = :closing
      @session.close if @session
      @state = :disconnected
    end

    def open(opts={})
      @opts   = SERVER_SOCKET_OPTS.merge(opts)
      @host   = @opts[:host]
      @port   = @opts[:port]
      @server = TCPServer.new(@host, @port)
      # @server.setsockopt(Socket::SOL_SOCKET, Socket::SO_RCVTIMEO, 5)
      #                   # @opts[:timeout])
      @state  = :listening
    end
    
    # Read one message unit. It's possible however that
    # more than one message will be set in a receive, so we will
    # have to buffer that for the next read.
    # EOFError will be raised on EOF.
    def read_msg
      wait_for_connect unless connected?
      while !@buf || @buf.empty?
        @buf, info = @session.recvfrom(TCP_MAX_PACKET)
      end
      @buf, data = unpack_msg(@buf)
      data
    end

    def wait_for_connect
      @input = @output = @session = @server.accept
      @state = :connected
    end
    
    # This method the debugger uses to write. In contrast to
    # writeline, no newline is added to the end to `str'. Also
    # msg doesn't have to be a string.
    def write(msg)
      wait_for_connect() unless connected?
      # FIXME: do we have to check the size of msg and split output? 
      @session.print(pack_msg(msg))
    end

    def writeline(msg)
      write(msg + "\n")
    end

  end
end

# Demo
if __FILE__ == $0
  include Trepanning::TCPPacking
  server = Trepan::TCPDbgServer.new({ :open => false,
                                      :port => 1027,
                                      :host => 'localhost'
                                    })
  if ARGV.size > 0
    puts 'Listening for connection...'
    server.open
    threads = []
    Thread.new do
      while true do
        begin
          line = server.read_msg.chomp
          puts "got #{line}"
        rescue EOFError
          puts 'Got EOF'
          break
        end
      end
    end
    threads << Thread.new do 
      t = TCPSocket.new('localhost', 1027)
      while true do
        begin
          print "input? "
          line = STDIN.gets
          break if !line || line.chomp == 'quit'
          t.puts(pack_msg(line))
        rescue EOFError
          puts "Got EOF"
          break
        rescue Exception => e
          puts "Got #{e}"
          break
        end
      end
      t.close
    end
    threads.each {|t| t.join }
    server.close
  end
end
