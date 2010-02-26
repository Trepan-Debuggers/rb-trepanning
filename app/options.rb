#!/usr/bin/env ruby
require 'optparse'
require 'ostruct'
module Rbdbgr

  VERSION = '0.01'
  PROGRAM = 'rbdbgr'

  def setup_options
    options = 
      OpenStruct.new(
                     'script' => nil,
                     'output' => nil,
                     )
    OptionParser.new do |opts|
      opts.banner = <<EOB
#{PROGRAM} version #{VERSION}
Usage: #{PROGRAM} [options] <script.rb> -- <script.rb parameters>
EOB
      opts.on("--script FILE", String, "Name of the script file to run") do |script| 
        unless File.exists?(script)
          puts "Script file '#{script}' is not found"
          exit
        end
      end
      opts.on("--output FILE", String, "Name of file to record output") do |outfile| 
      end
    end
  end
end

if __FILE__ == $0
  include Rbdbgr
  opts = setup_options
  opts.parse! ARGV
end
