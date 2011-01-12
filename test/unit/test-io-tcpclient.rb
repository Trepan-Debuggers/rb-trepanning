#!/usr/bin/env ruby
require 'test/unit'

# Unit test for io/tcpclient.rb
require_relative '../../io/tcpfns'
require_relative '../../io/tcpclient'

class TestTCPDbgClient < Test::Unit::TestCase

  include Trepanning::TCPPacking

  def test_basic
    client = Trepan::TCPDbgClient.new({ :open => false,
                                        :port => 1027,
                                        :host => 'localhost'
                                      })
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
      # require 'trepanning'; debugger
      3.times do 
        begin
          client.open
        rescue IOError
        end
        break
      end
      assert client
      msgs = %w(four five six)
      msgs.each do |mess|
        begin
          client.writeline(mess)
          assert_equal mess, client.read_msg.chomp
        rescue EOFError
          puts "Got EOF"
          break
        rescue Exception => e
          puts "Got #{e}"
          break
        end
      end
      client.close
    end
    threads.each {|t| t.join }
  end
end
