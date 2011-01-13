# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'optparse'
require_relative 'base/cmd'
require_relative '../../app/default'
require_relative '../../interface/server'   # server interface (remote debugging)
class Trepan::Command::ServerCommand < Trepan::Command
  include Trepanning

  unless defined?(HELP)
    CATEGORY     = 'support'
    DEFAULT_OPTIONS = {
      :host => DEFAULT_SETTINGS[:host],
      :port => DEFAULT_SETTINGS[:port]
    }
    MAX_ARGS     = 4  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [{--port|-p} NUM] [{--host|-h} HOST-OR-IP]

Put session into server mode which allows an out-of-process or remote
connection to the debugged program. --port and --host can be supplied
to specify the port number to use and the host name for TCP
connections. If neither is given, the default host (#{DEFAULT_SETTINGS[:host]}) 
and the default port (#{DEFAULT_SETTINGS[:port]}) are used.

Examples:
   #{NAME} # Accept remote connections using defaults
   #{NAME} --port 123 # Accept remote connections on port 123
   #{NAME} --host my.host.net --port 2048
   #{NAME} -h my.host.net -p 2048 # same as above
      HELP

    SHORT_HELP  = 'Go into out-of-process debugging (server) mode'

  end
    
  def parse_options(options, args) # nodoc
    parser = OptionParser.new do |opts|
      opts.on("-h", "--host NAME", String, 
              "Host or IP used in TCP connections for --server or --client. " + 
              "Default is #{DEFAULT_SETTINGS[:host].inspect}.") do 
        |name_or_ip| 
        options[:host] = name_or_ip
      end
      opts.on("-p", "--port NUMBER", Integer, 
              "Port number used in TCP connections for --server or --client. " + 
              "Default is #{DEFAULT_SETTINGS[:port]}.") do 
        |num| 
        options[:port] = num
      end
    end
    parser.parse(args)
    return options
  end

  # This method runs the command
  def run(args) # :nodoc
    options = parse_options(DEFAULT_OPTIONS.dup, args[1..-1])
    msg("starting debugger in out-of-process mode on host #{options[:host].inspect}, " +
        "port: #{options[:port]}")
    @proc.dbgr.intf << Trepan::ServerInterface.new(nil, nil, options)
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  # cmd.run([cmd.name])
  # cmd.run([cmd.name, '--server'])
end
