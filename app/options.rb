#!/usr/bin/env ruby
# Parses command-line options. 
require 'optparse'
module Rbdbgr

  VERSION = '0.01'
  PROGRAM = 'rbdbgr'

  def show_version
    "#{PROGRAM} version #{VERSION}"
  end

  def setup_options(options, stdout=$stdout, stderr=$stderr)
    OptionParser.new do |opts|
      opts.banner = <<EOB
#{show_version}
Usage: #{PROGRAM} [options] <script.rb> -- <script.rb parameters>
EOB
      opts.on("--command FILE", String, 
              "Execute debugger commnds from FILE") do |cmdfile| 
        unless File.readable?(cmdfile)
          if File.exists?
            stderr.puts "Command file '#{cmdfile}' is not readable."
          else
            stderr.puts "Command file '#{cmdfile}' does not exist."
          end
        end
        options[:cmdfiles] << cmdfile
      end
      opts.on("--output FILE", String, "Name of file to record output") do |outfile| 
        options[:outfile] = outfile
      end
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
  require_relative 'default'
  include Rbdbgr
  opts = {}
  options ={}
  # options = DEFAULT_CMDLINE_SETTINGS.merge({})
  [%w(--help), %w(--version)].each do |o|
    options ={}
    DEFAULT_CMDLINE_SETTINGS.each do |key, value|
      options[key] = value.clone if
        !value.nil? && value.respond_to?(:clone)
    end
    opts = setup_options(options)
    rest = opts.parse o
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
