# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>

# Our local modules
require_relative 'base/cmd'
require_relative '../../interface/script'
# Mfile     = import_relative('file', '...lib', 'pydbgr')

class Trepan::Command::SourceCommand < Trepan::Command
  unless defined?(HELP)
    NAME         = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [-v][-Y|-N][-c] FILE

Read debugger commands from a file named FILE.  Optional -v switch
(before the filename) causes each command in FILE to be echoed as it
is executed.  Option -Y sets the default value in any confirmation
command to be 'yes' and -N sets the default value to 'no'.

Note that the command startup file '.pydbgrc' is read automatically
via a #{NAME} command the debugger is started.

An error in any command terminates execution of the command file
unless option -c is given.
    HELP
    CATEGORY     = 'support'
    MIN_ARGS     = 1  # Need at least this many
    MAX_ARGS     = nil
    SHORT_HELP   = 'Read and run debugger commands from a file'
  end

  def run(args)
    verbose = false
    parms   = args[1..-1]
    opts    = {}
    parms.each do |arg|
      case arg
      when '-v'
        opts[:verbose]        = true
      when '-Y'
        opts[:confirm_val]    = true
      when '-N'
        opts[:confirm_val]    = false
      when '-c'
        opts[:abort_on_error] = false
      end
    end
    
    filename = args[-1]
    
    expanded_file = File.expand_path(filename)
    unless File.readable?(expanded_file)
      errmsg("Debugger command file '%s' is not a readable file" % filename)
      return false
    end
    
    # Push a new debugger interface.
    intf = @proc.dbgr.intf
    script_intf = Trepan::ScriptInterface.new(expanded_file,
                                              intf[-1].output,
                                              opts)
    intf << script_intf
    return false
  end
end
  
# Demo it
if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  if ARGV.size >= 1 
    puts "running... #{cmd.name} #{ARGV}"
    cmd.run([cmd.name, *ARGV])
  end
end
