class Debugger
  class CmdProcessor
    DEFAULT_SETTINGS = {
      :autoeval      => true,
      :autoirb       => false,
      :different     => true,  # stop *only* when  different position? 
      :'debug-skip'  => false,  
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
