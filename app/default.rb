# A place for the default settings
# This could be put elsewhere but it is expected that this will grow
# get quite large.
module Rbdbgr

  # I am not sure if we need to sets of hashes, but we'll start out
  # that way.

  # Default settings for a Debugger class object
  DEFAULT_SETTINGS = {
    :cmdproc_opts => {},  # Default Debugger::CmdProcessor settings
    :core_opts    => {},  # Default Debugger::Core settings
    :initial_dir  => nil, # Current directory run when "restart" is given
    :restart_argv => []   # Command run when "restart" is given
  }

  # Default settings for Debugger run from the command line.
  DEFAULT_CMDLINE_SETTINGS = {
    :cmdfiles => [],  # Initialization command files to run
    :output   => nil,
  }

end
if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(Rbdbgr::DEFAULT_SETTINGS)
  puts '=' * 30
  PP.pp(Rbdbgr::DEFAULT_CMDLINE_SETTINGS)
end
