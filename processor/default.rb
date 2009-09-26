class Debugger
  class CmdProcessor
    DEFAULT_SETTINGS = {
      :autoeval      => true,  # Ruby eval non-debugger commands
      :autoirb       => false, # Go into IRB in debugger command loop
      :autolist      => false, # Run 'list' 
      :basename      => false, # Show basename of filenames only
      :different     => true,  # stop *only* when  different position? 
      :'debug-skip'  => false, # Internal debugging 
      :listsize      => 10,    # Number of lines in list 
      :prompt        => '(rbdbgr): ',
      :stack_trace_on_error => false,
      :width         => (ENV['COLUMNS'] || '80').to_i,
    }
  end
end

if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(Debugger::CmdProcessor::DEFAULT_SETTINGS)
end
