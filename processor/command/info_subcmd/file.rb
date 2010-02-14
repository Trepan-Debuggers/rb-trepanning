# -*- coding: utf-8 -*-
require 'linecache'
require_relative %w(.. base subcmd)

class Debugger::Subcommand::InfoFile < Debugger::Subcommand
  unless defined?(HELP)
    HELP =
'info file [{FILENAME|.} [all | brkpts | iseq | sha1 | size | stat]]

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
'
    MIN_ABBREV   = 'fi'.size  # Note we have "info frame"
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = false
    PREFIX       = %w(info file)
  end
  
  # Get file information
  def run(args)
    return if args.size < 2
    args += %w(. size sha1) if args.size == 2
    filename = 
      if '.' == args[2]
        if not @proc.frame
          errmsg("No frame - no default file.")
          return false
          nil
        else
          @proc.frame.source_container[1]
        end
      else
        args[2]
      end

    m = filename + ' is'
    canonic_name = LineCache::map_file(filename)
    if LineCache::cached?(canonic_name)
      m += " cached in debugger"
      if canonic_name != filename
        m += (' as:' + canonic_name)
      end
      m += '.'
      msg(m)
    elsif SCRIPT_ISEQS__.member?(filename)
      msg(m + ' not previously cached in debugger.')
      msg('Instruction sequence for file found; now cached.')
      LineCache::cache(filename)
      canonic_name = filename
    else
      msg(m + ' not cached in debugger.')
      return
    end
    seen = {}
    args[3..-1].each do |arg|
      processed_arg = false

      if %w(all size).member?(arg) 
        unless seen[:size]
          max_line = LineCache::size(filename)
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
            msg("Instruction sequences inside file:")
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
  if  not (ARGV.size == 1 && ARGV[0] == 'noload')
    ISEQS__        = {}
    SCRIPT_ISEQS__ = {}
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    require_relative %w(.. .. mock)
    require_relative %w(.. .. subcmd)
    name = File.basename(__FILE__, '.rb')
    # FIXME: DRY the below code
    dbgr, cmd = MockDebugger::setup('info')
    subcommand = Debugger::Subcommand::InfoFile.new(cmd)
    testcmdMgr = Debugger::Subcmd.new(subcommand)
    

    subcommand.run(%w(info file nothere))
    puts '-' * 40
#    require_relative %w(.. .. .. lib rbdbgr)
#    dbgr = Debugger.new(:set_restart => true)
#    dbgr.debugger
    subcommand.run(%w(info file . all))
    puts '-' * 40
    subcommand.run(%w(info file . lines sha1 sha1))
  end
end
