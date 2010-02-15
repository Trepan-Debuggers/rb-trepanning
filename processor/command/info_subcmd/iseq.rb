# -*- coding: utf-8 -*-
require_relative %w(.. base subcmd)

class Debugger::Subcommand::InfoIseq < Debugger::Subcommand
  unless defined?(HELP)
    HELP         = 
'info iseq [METHOD|.]

Show information about an instruction sequence.

Examples:
  info iseq
  info iseq .
  info iseq require_relative
'
    MIN_ABBREV   = 'is'.size
    NAME         = File.basename(__FILE__, '.rb')
    NEED_STACK   = true
    PREFIX       = %w(info iseq)
    SHORT_HELP   = 'Information about an instruction sequence'
  end

  def run(args)
    iseq_name = args[2]
    if args.empty? || '.' == iseq_name
      iseq = frame = @proc.frame.iseq
    elsif !(matches = find_iseqs(iseq_name)).empty?
      # FIXME: do something if there is more than one
      iseq = matches[0]
    else
      iseq = @proc.method_iseq(iseq_name)
    end
    
    if iseq
      msg('Instruction sequence:')
      %w(name source_container arg_block arg_opts arg_post_len arg_rest 
         arg_simple argc arity iseq_size local_size orig
         ).each do |field|
        msg("\t#{field}: %s" % iseq.send(field).inspect)
      end
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
    else
      mess = "Can't find instruction sequence"
      mess += " for #{args.join(' ')}" unless args.empty?
      errmsg mess
    end
  end

end

if __FILE__ == $0
  # Demo it.
  ISEQS__ = {}

  require_relative %w(.. .. mock)
  require_relative %w(.. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('info')
  subcommand = Debugger::Subcommand::InfoIseq.new(cmd)
  testcmdMgr = Debugger::Subcmd.new(subcommand)

  subcommand.run_show_bool
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
end
