# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative './base/cmd'

class Trepan::Command::EditCommand < Trepan::Command

  old_verbose = $VERBOSE  
  $VERBOSE    = nil
  NAME        = File.basename(__FILE__, '.rb')
  HELP    = <<-HELP
#{NAME} [LOCATION]

With no argument, edits file containing most recent line listed.

Editing targets can also be specified.
      HELP

  ALIASES       = %w(e)
  CATEGORY      = 'files'
  NEED_STACK    = false
  SHORT_HELP    = 'Edit specified file or function'
  $VERBOSE      = old_verbose 

  def run(args)
    if args.size == 1

      unless true # @state.context
        errmsg "We are not in a state that has an associated file.\n"
        return 
      end
    else
      text = @proc.cmd_argstr
    end
    editor = ENV['EDITOR'] || '/bin/ex'
    file = @proc.frame.file
    if File.readable?(file)
      edit_cmd = "#{editor} +#{@proc.frame.line} \"#{file}\""
      msg "Running #{edit_cmd}..."
      system(edit_cmd)
      msg "Warning: return code was #{$?.exitstatus}" if $?.exitstatus != 0
    else
      errmsg "File \"#{file}\" is not readable.\n"
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  dbgr, cmd = MockDebugger::setup
  cmd.run [cmd.name] if ARGV.size > 0
end
