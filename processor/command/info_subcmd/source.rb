# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'pp'
require 'linecache'
require 'columnize'
require_relative '../base/subcmd'
require_relative '../../../app/file'
require_relative '../../../app/complete'

class Trepan::Subcommand::InfoSource < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    DEFAULT_FILE_ARGS = %w(size mtime sha1)

    HELP = <<-EOH
#{CMD} 

Show information about the current source file. 
EOH
    MAX_ARGS     = 0
    MIN_ABBREV   = 'so'.size  # Note we have "info frame"
    NEED_STACK   = true
  end

  # completion %w(all brkpts iseq sha1 size stat)

  include Trepanning

  # Get file information
  def run(args)
    if not @proc.frame
      errmsg('No frame - no default file.')
      return false
    end
    frame_file = @proc.frame.source_container[1]
    filename = LineCache::map_file(frame_file) || File.expand_path(frame_file)
    canonic_name = @proc.canonic_file(filename)
    canonic_name = LineCache::map_file(canonic_name) || canonic_name
    m = filename
    if LineCache::cached?(canonic_name)
      m += ' is cached in debugger'
      if canonic_name != filename
        m += (' as:\n  ' + canonic_name)
      end
      m += '.'
      msg(m)
    end
    max_line = LineCache::size(canonic_name)
    msg 'File has %d lines.' % max_line if max_line
    msg('SHA1 is %s.' % LineCache::sha1(canonic_name))
    msg('Possible breakpoint line numbers:')
    lines = LineCache.trace_line_numbers(canonic_name)
    fmt_lines = columnize_numbers(lines)
    msg(fmt_lines)
    if SCRIPT_ISEQS__.member?(canonic_name)
      msg("File contains instruction sequences:")
      SCRIPT_ISEQS__[canonic_name].each do |iseq|
        msg("\t%s %s" % [iseq, iseq.name.inspect])
      end 
    else
      msg("Instruction sequences not recorded; there may be some, though.")
    end
    msg("Stat info:\n\t%s" % LineCache::stat(canonic_name).pretty_inspect)
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
    cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoSource, false)
    cmd.run(cmd.prefix)
  end
end
