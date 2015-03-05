#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# Copyright (C) 2010-2011, 2013, 2015 Rocky Bernstein <rockyb@rubyforge.net>
#=== Summary
# Parses command-line options.

require 'optparse'
class Trepan
  require_relative 'default'

  VERSION = '0.2'
  PROGRAM = 'trepan'

  def self.show_version
    "#{PROGRAM}, version #{VERSION}"
  end

  def self.copy_default_options
    options = {}
    DEFAULT_CMDLINE_SETTINGS.each do |key, value|
      begin
        options[key] = value.clone
      rescue TypeError
        options[key] = value
      end
    end
    options
  end

  def self.setup_options(options, stdout=$stdout, stderr=$stderr)
      options[:highlight] = :term
      OptionParser.new do |opts|
          opts.banner = <<EOB
#{show_version}
Usage: #{PROGRAM} [options] [[--] <script.rb> <script.rb parameters>]
EOB
          opts.separator ''
          opts.separator 'Options:'
          opts.on('--basename',
                  'Show basename only on source file listings') do
              options[:basename] = true
          end
          opts.on('--client',
                  'Connect to out-of-process program') do
              if options[:server]
                  stderr.puts '--server option previously given. --client option ignored.'
              else
                  options[:client] = true
              end
          end
          opts.on('-c', '--command FILE', String,
                  'Execute debugger commands from FILE') do |cmdfile|
              if File.readable?(cmdfile)
                  options[:cmdfiles] << cmdfile
              elsif File.exists?(cmdfile)
                  stderr.puts "Command file '#{cmdfile}' is not readable. Option ignored."
              else
                  stderr.puts "Command file '#{cmdfile}' does not exist."
              end
          end
          opts.on('--cd DIR', String, 'Change current directory to DIR') do |dir|
              if File.directory?(dir)
                  if File.executable?(dir)
                      options[:chdir] = dir
                  else
                      stderr.puts "Can't cd to #{dir}. Option --cd ignored."
                  end
              else
                  stderr.puts "\"#{dir}\" is not a directory. Option --cd ignored."
              end
          end
          opts.on('-d', '--debug', "Set $DEBUG=true") {$DEBUG = true}
          opts.on('--[no-]highlight',
                  'Use [no] syntax highlight output') do |v|
              options[:highlight] = ((v) ? :term : nil)
          end
          opts.on('-h', '--host NAME', String,
                  'Host or IP used in TCP connections for --server or --client. ' +
                  "Default is #{DEFAULT_SETTINGS[:host].inspect}.") do
              |name_or_ip|
              options[:host] = name_or_ip
          end
          opts.on('-I', '--include PATH', String, 'Add PATH to $LOAD_PATH') do
              |path|
              $LOAD_PATH.unshift(path)
          end
          opts.on('--nx',
                  "Do not run debugger initialization file #{CMD_INITFILE}") do
              options[:nx] = true
          end
          opts.on('-p', '--port NUMBER', Integer,
                  'Port number used in TCP connections for --server or --client. ' +
                  "Default is #{DEFAULT_SETTINGS[:port]}.") do
              |num|
              options[:port] = num
          end
          # opts.on('-m', '--post-mortem', 'Activate post-mortem mode') do
          #   options[:post_mortem] = true
          # end
          opts.on('--[no-]readline',
                  'Try [not] GNU Readline') do |v|
              options[:readline] = v
          end
          opts.on('-r', '--require SCRIPT', String,
                  'Require the library, before executing your script') do |name|
              if name == 'debug'
                  stderr.puts "ruby-debug is not compatible with Ruby's 'debug' library. This option is ignored."
              else
                  require name
              end
          end
          opts.on('-s', '--server',
                  'Set up for out-of-process debugging') do
              if options[:client]
                  stderr.puts '--client option previously given. --server option ignored.'
              else
                  options[:server] = true
              end
          end
          opts.on('-x', '--trace', 'Turn on line tracing') do
              options[:traceprint] = true
              options[:nx] = true
          end
          opts.separator ''
          opts.on_tail('-?', '--help', 'Show this message') do
              options[:help] = true
              stdout.puts opts
              exit
          end
          opts.on_tail('-v', '--version',
                       'print the version') do
              options[:version] = true
              stdout.puts show_version
          end
      end
  end
end

if __FILE__ == $0
  opts = {}
  options ={}
  [%w(--help), %w(--version)].each do |o|
    options = Trepan::copy_default_options
    opts    = Trepan::setup_options(options)
    rest    = opts.parse o
    p options
    puts '=' * 10
  end
  rest = opts.parse! ARGV
  puts opts
  puts '=' * 10
  p options
  puts '=' * 10
  p Trepan::DEFAULT_CMDLINE_SETTINGS
end
