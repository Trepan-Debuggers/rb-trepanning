class Debugger
  attr_accessor :trace_filter # Procs/Methods we ignore.
  def initialize(opts={})
    @trace_filter = []
  end
  class MockCore
    attr_accessor :dbgr
    attr_reader   :core
    def event; 'line' end
  end 
end

