# -*- coding: utf-8 -*-
require_relative '../../base/subsubcmd'
require_relative '../substitute'

class Debugger::SubSubcommand::SetSubstitutePath < Debugger::SubSubcommand
  unless defined?(HELP)
    HELP         = 
'Add a substitution rule replacing FROM into TO in source file names.
If a substitution rule was previously set for FROM, the old rule
is replaced by the new one.'
    MIN_ABBREV   = 'fi'.size  
    MAX_ARGS     = 2
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP   = 'Use PATH in place of an filename'
    PREFIX       = %w(set substitute path)
  end

  def run(args)
    if args.size != 3
      errmsg "This command needs 2 arguments, got #{args.size-1}."
      return
    end
    from_path = args[1]
    to_path   = args[2]
    # FIXME Check from_path name to see if it is loaded
    if File.exist?(to_path)
      LineCache::remap_file(from_path, to_path)
    else
      errmsg "File #{to_path} doesn't exist"
    end
  end
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, set_cmd = MockDebugger::setup('set')
  testcmdMgr = Debugger::Subcmd.new(set_cmd)
  cmd_name   = Debugger::SubSubcommand::SetSubstitutePath::PREFIX.join('')
  setx_cmd   = Debugger::SubSubcommand::SetSubstitutePath.new(set_cmd.proc, 
                                                              set_cmd,
                                                              cmd_name)
  # require_relative '../../../../lib/rbdbgr'
  # dbgr = Debugger.new(:set_restart => true)
  # dbgr.debugger
  setx_cmd.run([])

  # name = File.basename(__FILE__, '.rb')
  # subcommand.summary_help(name)
end
