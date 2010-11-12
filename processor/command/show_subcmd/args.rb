# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowArgs < Trepan::Subcommand
  unless defined?(HELP)
    HELP = 'Show argument list to give program when it is restarted'
    MIN_ABBREV   = 'ar'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %W(show #{NAME})
  end

  def run(args)
    dbgr = @proc.dbgr
    msg "Restart directory: #{RubyVM::OS_STARTUP_DIR}"
    msg "Restart args:\n\t#{dbgr.restart_argv.inspect}"
  end
    
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  cmd_ary          = Trepan::Subcommand::ShowArgs::PREFIX
  dbgr, parent_cmd = MockDebugger::setup(cmd_ary[0], false)
  cmd              = Trepan::Subcommand::ShowArgs.new(parent_cmd)
  cmd.run([])
  cmd.summary_help(cmd.name)
  puts
end
