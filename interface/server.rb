# -*- coding: utf-8 -*-
# Copyright (C) 2011, 2012 Rocky Bernstein <rockyb@rubyforge.net>

# Our local modules
require_relative '../interface'
require_relative 'comcodes'
require_relative '../io/input'
require_relative '../io/tcpserver'

# Mfifoserver = import_relative('fifoserver', '..io', 'pydbgr')

# Interface for debugging a program but having user control
# reside outside of the debugged process, possibly on another
# computer
class Trepan::ServerInterface < Trepan::Interface

  include Trepanning::RemoteCommunication

  DEFAULT_INIT_CONNECTION_OPTS = {
    :io => 'TCP'
  } unless defined?(DEFAULT_INIT_CONNECTION_OPTS)

  def initialize(inout=nil, out=nil, connection_opts={})

    @connection_opts = DEFAULT_INIT_CONNECTION_OPTS.merge(connection_opts)

    at_exit { finalize }
    @inout = 
      if inout
        inout
      else
        server_type = @connection_opts[:io]
        # FIXME: complete this.
        # if 'FIFO' == server_type
        #     FIFOServer.new
        # else
        Trepan::TCPDbgServer.new(@connection_opts)
        # end
      end

    # In case we need to resync
    @last_action = :nop
    @last_args = []

    # For Compatability 
    @output = @inout
    @input  = @inout
    @interactive = true # Or at least so we think initially
  end
  
  # Closes both input and output
  def close
    if @inout && @inout.connected?
      @inout.write(QUIT + 'bye')
      @inout.close 
    end
  end
  
  def closed?
    @inout.closed?
  end

  # Called when a dangerous action is about to be done to make sure
  # it's okay. `prompt' is printed; user response is returned.
  # FIXME: make common routine for this and user.rb
  def confirm(prompt, default)
    while true
      begin
        write_confirm(prompt, default)
        reply = readline(nil).strip.downcase
      rescue EOFError
        return default
      end
      if YES.member?(reply)
        return true
      elsif NO.member?(reply)
        return false
      else
        msg "Please answer 'yes' or 'no'. Try again."
      end
    end
    @last_action = :confirm
    @last_args   = [prompt, default]
    return default
  end
  
  # Return true if we are connected
  def connected?
    :connected == @inout.state
  end
    
  # print exit annotation
  def finalize(last_wishes=QUIT)
    @inout.writeline(last_wishes) if connected?
    close
  end
  
  def input_eof?
    false
  end

  # used to write to a debugger that is connected to this
  # server; `str' written will have a newline added to it
  def msg(msg)
    @last_args   = [msg]
    @last_action = :msg
    @inout.writeline(PRINT + msg)
  end

  # used to write to a debugger that is connected to this
  # server; `str' written will not have a newline added to it
  def msg_nocr(msg)
    @last_args   = [msg]
    @last_action = :msg_nocr
    @inout.write(PRINT +  msg)
  end
  
  def read_command(prompt)
    @last_args   = [prompt]
    @last_action = :read_command
    readline(prompt)
  end
  
  def read_data
    @inout.read_dat
  end
  
  def nop; end

  def resync
    # puts "resync", @last_action, @last_args
    method(@last_action).call(*@last_args)
  end
  
  def readline(prompt, add_to_history=true)
    if prompt
      write_prompt(prompt)
    end
    
    coded_line = @inout.read_msg()
    while (SYNC == @read_ctrl = coded_line[0..0])
      resync
      coded_line = @inout.read_msg()
    end
    coded_line[1..-1]
  end
  
  # Return connected
  def state
    @inout.state
  end
  
  def write_prompt(prompt)
    @inout.write(PROMPT + prompt)
    @last_args   = [prompt]
    @last_action = :write_prompt
  end
  
  def write_confirm(prompt, default)
    if default
      code = CONFIRM_TRUE
    else
      code = CONFIRM_FALSE
    end
    @inout.write(code + prompt)
    @last_args   = [prompt, default]
    @last_action = :write_confirm
  end
end
    
# Demo
if __FILE__ == $0
  intf = Trepan::ServerInterface.new(nil, nil, :open => false)
  intf.resync
  def intf.msg(msg)
    @last_args   = [msg]
    @last_action = :msg
    puts msg
  end
  intf.msg('Hi!')
  intf.resync
end
