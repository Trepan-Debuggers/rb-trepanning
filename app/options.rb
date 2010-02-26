#!/usr/bin/env ruby
# Parses command-line options. 
require 'optparse'
module Rbdbgr

  VERSION = '0.01'
  PROGRAM = 'rbdbgr'

  def show_version
    #{PROGRAM} version #{VERSION}
  end

  def setup_options(options)
    OptionParser.new do |opts|
      opts.banner = <<EOB
#{show_version}
Usage: #{PROGRAM} [options] <script.rb> -- <script.rb parameters>
EOB
      opts.on("--script FILE", String, "Name of the script file to run") do |script| 
        unless File.exists?(script)
          puts "Script file '#{script}' is not found."
          exit
        end
        options[:script] = script
      end
      opts.on("--output FILE", String, "Name of file to record output") do |outfile| 
        options[:outtfile] = outfile
      end
      opts.on_tail("--help", "Show this message") do
        puts opts
        exit
      end
      opts.on_tail("--version", 
                   "print the version") do
        puts show_version
        exit
      end
    end
  end
end

if __FILE__ == $0
  require_relative 'default'
  options = Rbdbgr::DEFAULT_CMDLINE_SETTINGS
  include Rbdbgr
  opts = setup_options(options)
  rest = opts.parse! ARGV
  puts opts.to_s
  puts options
end
