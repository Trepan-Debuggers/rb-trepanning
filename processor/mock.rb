# Mock setup for commands.
require_relative 'main'
require_relative %w(.. lib core)

class MockDebugger
  attr_reader :core, :settings
  def initialize
    @settings = {
      :width => 80
    }
    @core = Debugger::Core.new(self)
  end
end
