# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'columnize'
require 'pp'
require_relative '../base/subcmd'
require_relative '../../../app/file'

class Trepan::Subcommand::InfoIseq < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP = <<-EOH
#{CMD} [METHOD|.]

Show information about an instruction sequence.

Examples:
  #{CMD}
  #{CMD} .
  #{CMD} *
  #{CMD} require_relative
  EOH
    MIN_ABBREV   = 'is'.size
    MIN_ARGS     = 0
    MAX_ARGS     = 1
    NEED_STACK   = true
    SHORT_HELP   = 'Information about an instruction sequence'
  end

  def iseq_list
    ISEQS__.keys
  end
  def complete(prefix)
    completions = ['.'] + iseq_list
    Trepan::Complete.complete_token(completions, prefix)
  end

  include Trepanning

  def run(args)
    args << '.' if 2 == args.size 
    iseq_name = args[2]
    if '*' == iseq_name
      section 'Instruction Sequences:'
      iseq_list.sort.each do |iseq|
        msg "\t #{iseq}"
      end
      return
    elsif '.' == iseq_name
      iseq = frame = @proc.frame.iseq
    elsif !(matches = find_iseqs(ISEQS__, iseq_name)).empty?
      # FIXME: do something if there is more than one
      iseq = matches[0]
    else
      iseq = nil
    end
    
    if iseq
      msg('Instruction sequence:')
      %w(name source_container arg_block arg_opts arg_post_len arg_rest 
         arg_simple argc arity iseq_size local_size orig
         ).each do |field|
        msg("\t#{field}: %s" % iseq.send(field).inspect)
      end
      msg("\tsha1: #{iseq.sha1}")
      if iseq.brkpts
        if iseq.brkpts.empty?
          msg("Breakpoints have been allocated, but none have been set.")
        else
          s = iseq.brkpts.size > 1 ? 's' : ''
          msg("Breakpoint%s at offset%s: %s" % [s, s, iseq.brkpts.join(', ')])
        end
      else
        msg("Breakpoints have not been allocated.")
      end
      if iseq.compile_options
        msg "\nCompile options:"
        msg iseq.compile_options.pretty_inspect
      end
      if iseq.eval_source
        msg "Source string:"
        msg @proc.safe_rep(iseq.eval_source.inspect)
      end
    else
      mess = "Can't find instruction sequence"
      mess += " for #{args.join(' ')}" unless args.empty?
      errmsg mess
    end
  end

end

if __FILE__ == $0
  # Demo it.
  ISEQS__        = {}
  SCRIPT_ISEQS__ = {} 

  require_relative '../../mock'
  require_relative '../../subcmd'

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Trepan::Subcommand::InfoIseq.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  def five; 5; end

  [%w(info iseq nothere),
   %w(info file .),
   %w(info file *),
  ].each do |args|
    subcommand.run(args)
    puts '-' * 40
  end
  p subcommand.complete('')
end
