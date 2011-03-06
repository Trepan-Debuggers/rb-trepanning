# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::SaveCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [--[no-]erase] [--output|-o FILENAME]

Save settings to file FILENAME. If FILENAME not given one will be made
selected.
    HELP

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Send debugger state to a file'

    DEFAULT_OPTIONS = { :erase => true, }
  end
    
  def parse_options(options, args) # :nodoc
    parser = OptionParser.new do |opts|
      opts.on("-e", "--[no-]erase", 
              "Add line to erase after reading") do
        |v| 
        options[:erase] = v
      end
      opts.on("-o", "--output FILE", String, 
              "Save file to FILE. ") do 
        |filename|
        options[:filename] = filename
      end
    end
    parser.parse(args)
    return options
  end

  # This method runs the command
  def run(args)
    options = parse_options(DEFAULT_OPTIONS.dup, args[1..-1])
    save_filename = @proc.save_commands(options)
    msg "Debugger commands written to file: #{save_filename}" if
      save_filename
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.run([cmd.name])
  # require_relative '../../lib/trepanning'; debugger
  cmd.run([cmd.name, '--erase', 
           '--output', File.join(Dir.tmpdir, 'save_file.txt')])
  # A good test would be to see we can read in those files without error.
end
