# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::ShowArgs < Trepan::Subcommand
  unless defined?(HELP)
    HELP = 'Show argument list to give program when it is restarted'
    MIN_ABBREV   = 'ar'.size
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(show args)
  end

  def run(args)
    dbgr = @proc.dbgr
    argv = dbgr.restart_argv
    msg("Restart directory: #{dbgr.initial_dir}") if dbgr.initial_dir
    msg("Restart args:\n\t#{argv.inspect}")
  end
    
end

if __FILE__ == $0
  # Demo it.
  require_relative '../../mock'
  require_relative '../../subcmd'
  name = File.basename(__FILE__, '.rb')

  # FIXME: DRY the below code
  dbgr, cmd = MockDebugger::setup('show')
  subcommand = Trepan::Subcommand::ShowArgs.new(cmd)
  testcmdMgr = Trepan::Subcmd.new(subcommand)

  def subcommand.msg(message)
    puts message
  end
  def subcommand.msg_nocr(message)
    print message
  end
  def subcommand.errmsg(message)
    puts message
  end
  subcommand.run([])
  name = File.basename(__FILE__, '.rb')
  subcommand.summary_help(name)
  puts
  dbgr.instance_variable_set('@initial_dir', Dir.pwd)
  dbgr.restart_argv = ARGV
  subcommand.run([])
  
end
