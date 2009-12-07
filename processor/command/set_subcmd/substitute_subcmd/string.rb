# -*- coding: utf-8 -*-
require 'tempfile'
require 'linecache'
require_relative %w(.. .. base subsubcmd)
require_relative %w(.. substitute)

class Debugger::SubSubcommand::SetSubstituteString < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 
'Add a substitution rule replacing FROM into TO in source file names.
If a substitution rule was previously set for FROM, the old rule
is replaced by the new one.'
    MIN_ABBREV   = 'st'.size  
    MAX_ARGS     = 2
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = 'Use STRING in place of an filename'
    PREFIX       = %w(set substitute string)
  end

  def run(args)
    if args.size != 3
      errmsg "This command needs 2 arguments, got #{args.size-1}."
      return
    end
    from_path = args[1]
    
    
    to_str = args[2]
    val = @proc.debug_eval_no_errmsg(to_str)

    if val
      tempfile = Tempfile.new(["#{from_path}-#{to_str}-", '.rb'])
      tempfile.open.puts(val)
      @proc.remap_container[['string', from_path]] = ['file', tempfile.path]
      tempfile.close
      LineCache::cache(tempfile.path)
    else
      errmsg "Can't get value for #{to_str}"
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative %w(.. .. .. mock)
  require_relative %w(.. .. .. subcmd)
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  testcmdMgr = Debugger::Subcmd.new(set_cmd)
  cmd_name   = Debugger::SubSubcommand::SetSubstituteString::PREFIX.join('')
  setx_cmd   = Debugger::SubSubcommand::SetSubstituteString.new(set_cmd.proc, 
                                                                set_cmd,
                                                                cmd_name)
  # require_relative %w(.. .. .. .. lib rbdbgr)
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  setx_cmd.run([])

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
