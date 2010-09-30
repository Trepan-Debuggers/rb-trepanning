# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
class Trepan
  attr_accessor :trace_filter # Procs/Methods we ignore.
  def initialize(opts={})
    @trace_filter = []
  end
  class MockDebugger
    attr_reader :initial_dir
    def initialize
      @initial_dir = '.'
    end
  end
  class MockCore
    attr_accessor :dbgr
    attr_reader   :core
    def initialize
      @dbgr = MockDebugger.new
    end
    def event; 'line' end
  end 
end

