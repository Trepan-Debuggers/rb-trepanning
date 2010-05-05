  require_relative '../app/default'
class Debugger
  class CmdProcessor

    DEFAULT_SETTINGS = {
      :autoeval      => true,      # Ruby eval non-debugger commands
      :autoirb       => false,     # Go into IRB in debugger command loop
      :autolist      => false,     # Run 'list' 

      :basename      => false,     # Show basename of filenames only
      :different     => 'nostack', # stop *only* when  different position? 

      :debugexcept   => true,      # Internal debugging of command exceptions
      :debugmacro    => false,     # debugging macros
      :debugskip     => false,     # Internal debugging of step/next skipping
      :debugstack    => false,     # How hidden outer debugger stack frames

      :listsize      => 10,        # Number of lines in list 
      :maxstack      => 16,        # backtrace limit
      :maxstring     => 150,       # Strings which are larger than this
                                   # will be truncated to this length when
                                   # printed
      :maxwidth       => (ENV['COLUMNS'] || '80').to_i,
      :prompt        => '(rbdbgr): ',
      :save_cmdfile  => nil,       # If set, debugger command file to be
                                   # used on restart
      :timer         => false,     # show elapsed time between events
      :traceprint    => false,     # event tracing printing
      :tracebuffer   => false,     # save events to a trace buffer.
      :user_cmd_dir  => File.join(Rbdbgr::HOME_DIR, 'rbdbgr', 'command'),
                                   # User command directory
    } unless defined?(DEFAULT_SETTINGS)
  end
end

if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(Debugger::CmdProcessor::DEFAULT_SETTINGS)
end
