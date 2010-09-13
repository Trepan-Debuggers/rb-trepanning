# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
# A place for the default settings
# This could be put elsewhere but it is expected that this will grow
# get quite large.
module Rbdbgr

  # I am not sure if we need to sets of hashes, but we'll start out
  # that way.

  # Default settings for a Debugger class object
  DEFAULT_SETTINGS = {
    :cmdproc_opts    => {},    # Default Debugger::CmdProcessor settings
    :core_opts       => {},    # Default Debugger::Core settings
    :delete_restore  => true,  # Delete restore profile after reading? 
    :initial_dir     => nil,   # Current directory run when "restart" is given
    :nx              => false, # Don't run user startup file (e.g. .rbdbgrc)
    :restart_argv    => [],    # Command run when "restart" is given
    :restore_profile => nil    # Profile used to set/restore debugger state
  } unless defined?(DEFAULT_SETTINGS)

  # Default settings for Debugger run from the command line.
  DEFAULT_CMDLINE_SETTINGS = {
    :cmdfiles => [],  # Initialization command files to run
    :nx       => false, # Don't run user startup file (e.g. .rbdbgrc)
    :output   => nil,
  } unless defined?(DEFAULT_CMDLINE_SETTINGS)

  DEFAULT_DEBUG_STR_SETTINGS = {
    :core_opts => {
      :cmdproc_opts => {:different => false}},
    :hide_stack => true,
  } unless defined?(DEFAULT_DEBUG_STR_SETTINGS)

  CMD_INITFILE_BASE = 
    if RUBY_PLATFORM =~ /mswin/
      # Of course MS Windows has to be different
      HOME_DIR     =  (ENV['HOME'] || 
                       ENV['HOMEDRIVE'].to_s + ENV['HOMEPATH'].to_s).to_s
      'rbdbgr.ini'
    else
      HOME_DIR = ENV['HOME'].to_s
      '.rbdbgrc'
    end unless defined?(CMD_INITFILE_BASE)
  CMD_INITFILE = File.join(HOME_DIR, CMD_INITFILE_BASE) unless
    defined?(CMD_INITFILE)
end

if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(Rbdbgr::DEFAULT_SETTINGS)
  puts '=' * 30
  PP.pp(Rbdbgr::DEFAULT_CMDLINE_SETTINGS)
end
