class Debugger
  class CmdProcessor
    DEFAULT_SETTINGS = {
      :autoeval      => true,
      :stack_trace_on_error => false,
      :prompt        => '(rbdbgr): ',
      :width         => (ENV['COLUMNS'] || '80').to_i
    }
  end
end

if __FILE__ == $0
  # Show it:
  require 'pp'
  PP.pp(Debugger::CmdProcessor::DEFAULT_SETTINGS)
end
