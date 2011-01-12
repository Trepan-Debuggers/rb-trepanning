#!/usr/bin/env ruby
require 'test/unit'

# Unit test for io/tcpclient.rb and io/tcpserver.rb together
require_relative '../../io/tcpfns'
require_relative '../../io/tcpclient'
require_relative '../../io/tcpserver'

# Tests TCPServer together with and TCPClient
class TestTCPClientServer < Test::Unit::TestCase

  def test_client_server
    server = Trepan::TCPDbgServer.new({ :open => true,
                                        :port => 1028,
                                        :host => 'localhost'
                                      })
    client = Trepan::TCPDbgClient.new({ :open => true,
                                        :port => 1028,
                                        :host => 'localhost'
                                      })
    %w(one, two, three).each do |line|
      server.writeline(line)
      assert_equal line, client.read_msg.chomp
    end
    %w(four, five, six).each do |line|
      client.writeline(line)
      assert_equal line, server.read_msg.chomp
    end
    client.close
    server.close
  end
end
