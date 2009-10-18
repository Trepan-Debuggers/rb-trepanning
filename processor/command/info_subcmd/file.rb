# -*- coding: utf-8 -*-
require 'linecache'
require_relative %w(.. base subcmd)

class Debugger::Subcommand::InfoFile < Debugger::Subcommand
  unless defined?(HELP)
    HELP =
'info file [{FILENAME|.} [all | brkpts | sha1 | size]]

Show information about the current file. If no filename is given and
the program is running then the current file associated with the
current stack entry is used. Sub options which can be shown about a file are:

brkpts -- Line numbers where there are statement boundaries. 
          These lines can be used in breakpoint commands.
sha1  -- A SHA1 hash of the source text. This may be useful in comparing
         source code.
size  -- The number of lines in the file.
stat  -- File.stat information

all   -- All of the above information.
'
    MIN_ABBREV   = 'fi'.size  # Note we have "info frame"
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = false
    PREFIX       = %w(info file)
  end
  
  # Get file information
  def run(args)
    if args.size == 0
      if not @proc.frame
        errmsg("No frame - no default file.")
        return false
      end
      filename = @proc.frame.source_container[1]
    else
      filename = ('.' == args[0]) ? @proc.frame.source_container[1] : args[0]
    end
    
    m = filename + ' is'
    if LineCache::cached_script?(filename)
      canonic_name = LineCache::unmap_file(filename)
      m += " cached in debugger"
      if canonic_name != filename
        m += (' as:' + canonic_name)
      end
      m += '.'
      msg(m)
    else
      msg(m + ' not cached in debugger.')
      return
    end
    args[1..-1].each do |arg|
      processed_arg = false

      if %w(all size).member?(arg)
        max_line = LineCache::size(filename)
        msg "File has %d lines." % max_line if max_line
        processed_arg = true
      end

      if %w(all sha1).member?(arg)
        msg("SHA1 is %s." % LineCache::sha1(filename))
        processed_arg = true
      end
      if %w(all brkpts).member?(arg)
        msg("Possible breakpoint line numbers:")
        lines = LineCache::trace_line_numbers(filename)
        fmt_lines = columnize_commands(lines)
        msg(fmt_lines)
        processed_arg = true
      end
      if %w(all stat).member?(arg)
        msg("stat info\n\t%s." % LineCache::stat(filename).inspect)
        processed_arg = true
      end
      if not processed_arg
        errmsg("Don't understand sub-option %s." % arg)
      end
    end unless args.empty?
  end
end

if __FILE__ == $0
  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')
  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoFile.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run([])
  LineCache::cache(__FILE__)
  subcommand.run(%w(. all))
  # sub.run(['file.py', 'all'])
  # sub.run(['file.py', 'lines', 'sha1'])
end
