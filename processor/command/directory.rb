# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative 'base/cmd'
class Trepan::Command::DirectoryCommand < Trepan::Command

  unless defined?(HELP)
    ALIASES      = %w(dir)
    CATEGORY     = 'files'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
Add directory DIR to beginning of search path for source files.
DIR can also be $cwd for the current working directory, or $cdir for the
directory in which the debugged file start.
With no argument, reset the search path to $cdir:$cwd, the default.

This command may be useful for debugging into Rubinius methods such as
kernel/common/module.rb if have the source code somewhere.
      HELP
    SHORT_HELP  = 
      'Add directory DIR to beginning of search path for source files'
  end
    
  # This method runs the command
  def run(args)
    if args.size > 1
      settings[:directory] = "#{args[1]}:#{settings[:directory]}"
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
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
  cmd.run([name])
  cmd.run([name, '/tmp'])
end
