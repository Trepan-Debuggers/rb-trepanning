# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'linecache'
require_relative '../base/subcmd'
require_relative '../../../app/file'

class Trepan::Subcommand::InfoFile < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    DEFAULT_FILE_ARGS = %w(size sha1)

    HELP =
"info file [{FILENAME|.} [all | brkpts | iseq | sha1 | size | stat]]

Show information about the current file. If no filename is given and
the program is running then the current file associated with the
current stack entry is used. Sub options which can be shown about a file are:

brkpts -- Line numbers where there are statement boundaries. 
          These lines can be used in breakpoint commands.
iseq   -- Instruction sequences from this file.
sha1   -- A SHA1 hash of the source text. This may be useful in comparing
          source code.
size   -- The number of lines in the file.
stat   -- File.stat information

all    -- All of the above information.

If no sub-options are given #{DEFAULT_FILE_ARGS.join(' ')} are assumed.
"
    MIN_ABBREV   = 'fi'.size  # Note we have "info frame"
    NEED_STACK   = false
  end

  completion %w(all brkpts iseq sha1 size stat)

  include Trepanning
  
  # Get file information
  def run(args)
    return if args.size < 2
    args << '.' if 2 == args.size 
    filename = 
      if '.' == args[2]
        if not @proc.frame
          errmsg("No frame - no default file.")
          return false
          nil
        else
          File.expand_path(@proc.frame.source_container[1])
        end
      else
        args[2]
      end
    args += DEFAULT_FILE_ARGS if args.size == 3

    m = filename + ' is'
    canonic_name = LineCache::map_file(filename)
    if LineCache::cached?(canonic_name)
      m += " cached in debugger"
      if canonic_name != filename
        m += (' as:' + canonic_name)
      end
      m += '.'
      msg(m)
    elsif !(matches = find_scripts(filename)).empty?
      if (matches.size > 1)
        msg("Multiple files found:")
        matches.each { |match_file| msg "\t%s" % match_file }
        return
      else
        msg('File "%s" just now cached.' % filename)
        LineCache::cache(matches[0])
        LineCache::remap_file(matches[0], filename)
        canonic_name = matches[0]
      end
    else
      msg(m + ' not cached in debugger.')
      return
    end
    seen = {}
    args[3..-1].each do |arg|
      processed_arg = false

      if %w(all size).member?(arg) 
        unless seen[:size]
          max_line = LineCache::size(canonic_name)
          msg "File has %d lines." % max_line if max_line
        end
        processed_arg = seen[:size] = true
      end

      if %w(all sha1).member?(arg)
        unless seen[:sha1]
          msg("SHA1 is %s." % LineCache::sha1(canonic_name))
        end
        processed_arg = seen[:sha1] = true
      end

      if %w(all brkpts).member?(arg)
        unless seen[:brkpts]
          msg("Possible breakpoint line numbers:")
          lines = LineCache::trace_line_numbers(canonic_name)
          fmt_lines = columnize_numbers(lines)
          msg(fmt_lines)
        end
        processed_arg = seen[:brkpts] = true
      end

      if %w(all iseq).member?(arg) 
        unless seen[:iseq]
          if SCRIPT_ISEQS__.member?(canonic_name)
            msg("File contains instruction sequences:")
            SCRIPT_ISEQS__[canonic_name].each do |iseq|
              msg("\t %s %s" % [iseq, iseq.name.inspect])
            end 
          else
            msg("Instruction sequences not recorded; there may be some, though.")
          end
        end
        processed_arg = seen[:iseq] = true
      end

      if %w(all stat).member?(arg)
        unless seen[:stat]
          msg("Stat info:\n\t%s." % LineCache::stat(canonic_name).inspect)
        end
        processed_arg = seen[:stat] = true
      end

      if not processed_arg
        errmsg("I don't understand sub-option \"%s\"." % arg)
      end
    end unless args.empty?
  end
end

if __FILE__ == $0
  if !(ARGV.size == 1 && ARGV[0] == 'noload')
    ISEQS__        = {}
    SCRIPT_ISEQS__ = {}
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    require_relative '../../mock'
    require_relative '../../subcmd'
    name = File.basename(__FILE__, '.rb')
    # FIXME: DRY the below code
    dbgr, cmd = MockDebugger::setup('info')
    subcommand = Trepan::Subcommand::InfoFile.new(cmd)
    testcmdMgr = Trepan::Subcmd.new(subcommand)
    
    [%w(info file nothere),
     %w(info file .),
     %w(info file),
     %w(info file file.rb),
     %w(info file . all),
     %w(info file . lines size sha1 sha1)].each do |args|
      subcommand.run(args)
      puts '-' * 40
    end
  end
end
