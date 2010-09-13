#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
#=== Summary
# Parses command-line options. 
#=== Options
#
#<tt>--cd=DIR</tt>
#      Change current directory to DIR.
#
#<tt>--command</tt> <i>file</i>::
#      Run debugger command file <i>file</i>
#
#<tt>--nx</tt>::
#      Don’t execute commands  found in any initialization
#      files, e.g. <tt>.rdebugrc</tt>.
#
#<tt>--restore=PROFILE</tt>::
#      Debugger command file which restores debugger settings,
#      resumably saved before a 'restart' debugger command.
#
#<tt>--version</tt>::
#      Show the version number.
#
#<tt>--help</tt>::
#      Show invocation help and exit.

require 'optparse'
module Rbdbgr
  require_relative 'default'

  VERSION ||= '0.01'
  PROGRAM ||= 'rbdbgr'

  def show_version
    "#{PROGRAM} version #{VERSION}"
  end

  def copy_default_options
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

  def setup_options(options, stdout=$stdout, stderr=$stderr)
    OptionParser.new do |opts|
      opts.banner = <<EOB
#{show_version}
Usage: #{PROGRAM} [options] <script.rb> -- <script.rb parameters>
EOB
      opts.on('--command FILE', String, 
              "Execute debugger commnds from FILE") do |cmdfile| 
        if File.readable?(cmdfile)
          options[:cmdfiles] << cmdfile
        elsif File.exists?(cmdfile)
            stderr.puts "Command file '#{cmdfile}' is not readable."
        else
          stderr.puts "Command file '#{cmdfile}' does not exist."
        end
      end
      opts.on('--nx',
              'Not run debugger initialization files (e.g. .rbdbgrc') do
        options[:nx] = true
      end
      # opts.on('--output FILE', String, "Name of file to record output") do |outfile| 
      #   options[:outfile] = outfile
      # end
      opts.on("--cd DIR", String, "Change current directory to DIR") do |dir| 
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
      opts.on("--restore PROFILE", String, 
              "Restore debugger state using PROFILE") do |profile|
        if File.readable?(profile)
          options[:restore_profile] = profile
          stderr.puts "Debugger command file #{profile} is not readable. --restore option ignored."
        end
      end
      opts.on_tail("--help", "Show this message") do
        options[:help] = true
        stdout.puts opts
      end
      opts.on_tail("--version", 
                   "print the version") do
        options[:version] = true
        stdout.puts show_version
      end
    end
  end
end

if __FILE__ == $0
  include Rbdbgr
  opts = {}
  options ={}
  [%w(--help), %w(--version)].each do |o|
    options = copy_default_options
    opts    = setup_options(options)
    rest    = opts.parse o
    puts options
    puts '=' * 10
  end
  rest = opts.parse! ARGV
  puts opts.to_s
  puts '=' * 10
  puts options
  puts '=' * 10
  puts Rbdbgr::DEFAULT_CMDLINE_SETTINGS
end
