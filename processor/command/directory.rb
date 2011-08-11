# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../command'
class Trepan::Command::DirectoryCommand < Trepan::Command

  unless defined?(HELP)
    ALIASES      = %w(dir)
    CATEGORY     = 'files'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [DIR]

Add directory DIR to beginning of search path for source files.
DIR can also be $cwd for the current working directory, or $cdir for the
directory in which the debugged file start.
With no argument, reset the search path to $cdir:$cwd, the default.

This command may be useful if for some reason the debugger can't find 
source files because directories have been moved. 

Examples:
   #{NAME} ~/.rvm/src/ruby-head  # Adds an rvm-like directory to path
   #{NAME} # reset to $cdir:$cwd
      HELP

    SHORT_HELP  = 
      'Add directory DIR to beginning of search path for source files'
  end
    
  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
      path = File.expand_path(args[1])
      settings[:directory] = "#{path}:#{settings[:directory]}"
      msg "Source directories searched: #{settings[:directory]}"
    else
      if confirm('Reintialize source path to empty?', false)
        settings[:directory] = '$cdir:$cwd'
        msg 'Source directories searched: $cdir:$cwd'
      end
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  cmd.run([cmd.name])
  cmd.run([cmd.name, '/tmp'])
end
