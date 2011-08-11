# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'optparse'

# Our local modules
require_relative '../command'
require_relative '../../interface/script'
require_relative '../../io/null_output'
require_relative '../../app/default'

class Trepan::Command::SourceCommand < Trepan::Command
  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [options] FILE

options: 
    -q | --quiet | --no-quiet
    -c | --continue | --no-continue
    -Y | --yes | -N | --no
    -v | --verbose | --no-verbose

Read debugger commands from a file named FILE.  Optional -v switch
causes each command in FILE to be echoed as it is executed.  Option -Y
sets the default value in any confirmation command to be 'yes' and -N
sets the default value to 'no'.

Option -q will turn off any debugger output that normally occurs in the
running of the program.

Note that the command startup file #{Trepan::CMD_INITFILE_BASE} is read automatically
via a #{NAME} command the debugger is started.

An error in any command terminates execution of the command file
unless option -c or --continue is given.
    HELP
    CATEGORY     = 'support'
    MIN_ARGS     = 1  # Need at least this many
    MAX_ARGS     = nil
    SHORT_HELP   = 'Read and run debugger commands from a file'

    DEFAULT_OPTIONS = {
      :abort_on_error => false,
      :confirm_val => false,
      :quiet => false,
      :verbose => false
    }

  end

  def complete(prefix)
    files = Readline::FILENAME_COMPLETION_PROC.call(prefix) || []
    opts = %w(-c --continue --no-continue -N --no -y --yes
              --verbose --no-verbose) + files
    Trepan::Complete.complete_token(opts, prefix) 
  end
    
  def parse_options(options, args) # :nodoc
    seen_yes_no = false
    parser = OptionParser.new do |opts|
      opts.on('-c', '--[no-]continue', 
              'Continue in the face of errors') do
        |v| 
        options[:abort_on_error] = !v
      end
      opts.on('-v', 
              '--[no-]verbose', 'echo each command as it is executed') do
        |v| 
        options[:verbose] = v
      end
      opts.on('-N', '--no', "Use 'no' in any confirmation prompts") do
        |v| 
        if seen_yes_no
          msg('Yes/No option already seen. This option (no) ignored.')
        end
        options[:confirm_val] = false
      end
      opts.on('-q', '--[no-]quiet', 'Silence debugger output') do
        |v| 
        options[:quiet] = v
      end
      opts.on('-Y', '--yes', "Use 'yes' in any confirmation prompts") do
        |v| 
        if seen_yes_no
          msg("Yes/No option already seen. This option, --yes, ignored.")
        end
        options[:confirm_val] = true
        seen_yes_no = true
      end
    end
    parser.parse(args)
    return options
  end

  def run(args)
    options = parse_options(DEFAULT_OPTIONS.dup, args[1..-2])
    intf = @proc.interfaces
    output  = options[:quiet] ? Trepan::OutputNull.new(nil) : intf[-1].output
    
    filename = args[-1]
    
    expanded_file = File.expand_path(filename)
    unless File.readable?(expanded_file)
      errmsg("Debugger command file '%s' (%s) is not a readable file" % 
             [filename, expanded_file])
      return false
    end
    
    # Push a new debugger interface.
    script_intf = Trepan::ScriptInterface.new(expanded_file, output, options)
    intf << script_intf
    return false
  end
end
  
# Demo it
if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  %w(--quiet -q --no-quiet --continue --no-continue -c -v --verbose 
     --no-verbose).each do |opt|
    puts "parsing #{opt}"
    options = 
      cmd.parse_options(Trepan::Command::SourceCommand::DEFAULT_OPTIONS.dup,
                        opt)
    p options
  end

  if ARGV.size >= 1 
    puts "running... #{cmd.name} #{ARGV}"
    cmd.run([cmd.name, *ARGV])
  end
end
