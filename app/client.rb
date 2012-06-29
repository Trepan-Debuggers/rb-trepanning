# -*- coding: utf-8 -*-
# Copyright (C) 2011, 2012 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'default'                # default debugger settings
require_relative '../lib/trepanning'      # main Trepan object & initialization
require_relative '../interface/comcodes'  # communication codes

module Trepanning
  include Trepanning::RemoteCommunication
  def start_client(options)
    puts "Client option given"
    user_opts = {}
    %w(readline).each do |opt|
      user_opts[opt.to_sym] = options[opt.to_sym]
    end
    dbgr = Trepan.new(:client      => true,
                      :cmdfiles    => [],
                      :initial_dir => options[:chdir],
                      :nx          => true,
                      :host        => options[:host],
                      :port        => options[:port],
                      :user_opts   => user_opts
                      )
    intf = dbgr.intf[-1]
    intf.write_remote(SYNC, 'FIXME: add useful info')
    while true
      begin
        control_code, line = intf.read_remote
      rescue EOFError, Errno::EPIPE
        puts "Remote debugged process closed connection"
        break
      end
      # p [control_code, line]
      case control_code
      when PRINT

        # FIXME: don't know why server sometimes adds a gratuituous space.
        # the space is added somewhere inside TCPSocket.print
        line = line[0..-2] if line.end_with?("\n ")

        print line
      when CONFIRM_TRUE
        response = intf.confirm(line, true)
        intf.write_remote(CONFIRM_REPLY, response ? 'Y' : 'N')
      when CONFIRM_FALSE
        response = intf.confirm(line, true)
        intf.write_remote(CONFIRM_REPLY, response ? 'Y' : 'N')
      when PROMPT
        # Printing of prompt has been handled already by PRINT.
        begin
          command = intf.read_command(line)
        rescue EOFError
          puts "user-side EOF. Quitting..."
          break
        end
        begin 
          intf.write_remote(COMMAND, command)
        rescue Errno::EPIPE
          puts "Remote debugged process died"
          break
        end
      when QUIT
        break
      when RESTART
        break
      else
        $stderr.puts "** Unknown control code: #{control_code}"
      end
    end
  end
  module_function :start_client
end
