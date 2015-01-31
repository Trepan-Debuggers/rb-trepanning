# -*- coding: utf-8 -*-
# Copyright (C) 2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative './../command'

class Trepan::Command::EditCommand < Trepan::Command

  old_verbose = $VERBOSE
  $VERBOSE    = nil
  NAME        = File.basename(__FILE__, '.rb')
  HELP    = <<-HELP
**#{NAME}** [[*file*] [*line*]]

With no argument, edits file containing most recent line listed.
The value of the environment variable `EDITOR` is used for the
editor to run. If no `EDITOR` environment variable is set `/bin/ex`
is used. The editor should support line and file positioning via:

    editor-name +line file-name

(Most editors do.)

Examples:
---------

    #{NAME}            # Edit current location
    #{NAME} 7          # Edit current file at line 7
    #{NAME} test.rb    # Edit test.rb, line 1
    #{NAME} test.rb 10 # Edit test.rb  line 10
      HELP

  ALIASES       = %w(e)
  CATEGORY      = 'files'
  NEED_STACK    = false
  SHORT_HELP    = 'Invoke an editor on some source code'
  MAX_ARGS      = 2
  $VERBOSE      = old_verbose

  # FIXME: redo with locations and kparse.
  def run(args)
    case args.size
    when 1
      file = @proc.frame.source_container[1]
      line = @proc.frame.source_location[0]
    when 2
      line = Integer(args[1]) rescue nil
      if line
        file = @proc.frame.source_container[1]
      else
        file = args[1]
        line = 1
      end
    when 3
      line, file =  args[2], args[1]
    else
      errmsg "edit needs at most 2 args."
    end
    editor = ENV['EDITOR'] || '/bin/ex'
    unless File.executable?(editor)
      errmsg "Editor #{editor} is not executable. Trying anyway..."
    end

    if File.readable?(file)
      file = File.basename(file) if settings[:basename]
      edit_cmd = "#{editor} +#{line} \"#{file}\""
      msg "Running #{edit_cmd}..."
      system(edit_cmd)
      msg "Warning: return code was #{$?.exitstatus}" if $?.exitstatus != 0
    else
      errmsg "File \"#{file}\" is not readable."
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  ENV['EDITOR'] = 'echo FAKE-EDITOR'
  cmd.run [cmd.name]
  cmd.run [cmd.name, '7']
  cmd.run [cmd.name, __FILE__, '10']
end
