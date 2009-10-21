class Debugger
  class CmdProcessor
    DEFAULT_SETTINGS = {
      :autoeval      => true,  # Ruby eval non-debugger commands
      :autoirb       => false, # Go into IRB in debugger command loop
      :autolist      => false, # Run 'list' 

      :basename      => false, # Show basename of filenames only
      :different     => true,  # stop *only* when  different position? 

      :debugexcept   => true,  # Internal debugging of command exceptions
      :debugskip     => false, # Internal debugging of step/next skipping
      :debugstack    => false, # How hidden outer debugger stack frames

      :listsize      => 10,    # Number of lines in list 
      :maxstring     => 150,   # Strings which are larger than this
                               # will be truncated to this length when
                               # printed
      :prompt        => '(rbdbgr): ',
      :width         => (ENV['COLUMNS'] || '80').to_i,
    }
  end
end

if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(Debugger::CmdProcessor::DEFAULT_SETTINGS)
end
