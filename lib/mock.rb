class Debugger
  class MockCore
    attr_accessor :dbgr
    attr_reader   :core
    def event; 'line' end
  end 
end

