# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'default'                # default debugger settings
require_relative '../interface/comcodes'  # communication codes

module Trepanning
  include Trepanning::RemoteCommunication
  def start_client(options)
    puts "Client option given"
    dbgr = Trepan.new(:client      => true,
                      :cmdfiles    => [],
                      :initial_dir => options[:chdir],
                      :nx          => true,
                      :host        => options[:host],
                      :port        => options[:port]
                      )
    intf = dbgr.intf[-1]
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
        print line
      when CONFIRM_TRUE
        response = intf.confirm(line, true)
        intf.write_remote(CONFIRM_REPLY, response ? 'Y' : 'N')
      when CONFIRM_FALSE
        response = intf.confirm(line, true)
        intf.write_remote(CONFIRM_REPLY, response ? 'Y' : 'N')
      when PROMPT
        # require 'trepanning'
        # debugger
        command = intf.read_command(line)
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
