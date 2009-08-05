# A place for the debugger default settings.
# This could be put in rbdbgr, but it is expected that this will
# get quite large.
module DbgSettings
  DEFAULT_SETTINGS = {
    :cmdproc_opts => {},  # Default Debugger::CmdProcessor settings
    :core_opts    => {}  #  Default Debugger::Core settings
  }

end
if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(DbgSettings::DEFAULT_SETTINGS)
end
