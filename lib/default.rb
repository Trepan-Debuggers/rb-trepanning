# A place for the debugger default settings.
# This could be put in rbdbgr, but it is expected that this will
# get quite large.
module DbgSettings
  DEFAULT_SETTINGS = {
    'width'         => (ENV['COLUMNS'] || '80').to_i
  }

end
if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(DbgSettings::DEFAULT_SETTINGS)
end
