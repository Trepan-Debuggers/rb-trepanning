# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Interface for client (i.e. user to communication-device) interaction.
# The debugged program is at the other end of the communcation.

require_relative 'user'
require_relative '../io/tcpclient'
require_relative 'comcodes'

# Mfifoclient = import_relative('fifoclient', '..io', 'pydbgr')

# Interface for a user which is attached to a debugged process via
# some sort of communication medium (e.g. socket, tty, FIFOs).  This
# could be on the same computer in a different process or on a remote
# computer.
class Trepan::ClientInterface < Trepan::Interface

  DEFAULT_INIT_CONNECTION_OPTS = {
    :open => true,
    :io   => :tcp
  }

  def initialize(inp=nil, out=nil, inout=nil, user_opts={}, 
                 connection_opts={})

    @connection_opts = DEFAULT_INIT_CONNECTION_OPTS.merge(connection_opts)

    @user = Trepan::UserInterface.new(inp, out, user_opts)
    
    @inout = 
      if inout
        inout 
      else
        # @server_type = @connection_opts[:io]
        # if 'FIFO' == self.server_type
        #   Mfifoclient.FIFOClient(opts=@connection_opts)
        # elsif :tcp == self.server_type
        Trepan::TCPDbgClient.new(@connection_opts)
        # else
        #   errmsg("Expecting server type TCP or FIFO. Got: %s." %
        #          self.server_type)
        #   return
        # end
      end
  end

  def confirm(prompt, default)
    @user.confirm(prompt, default)
  end

  def read_command(prompt='')
    @user.read_command(prompt)
  end

  # Send a message back to the server (in contrast to the local user
  # output channel).
  def read_remote
    coded_line = nil
    until coded_line
      coded_line = @inout.read_msg
    end
    control = coded_line[0..0]
    remote_line = coded_line[1..-1]
    [control, remote_line]
  end

  # Send a message back to the server (in contrast to the local user
  # output channel).
  def write_remote(code, msg)
    # FIXME change into write_xxx
    @inout.writeline(code + msg)
  end
end
  
# Demo
if __FILE__ == $0
    intf = Trepan::ClientInterface.new
end
