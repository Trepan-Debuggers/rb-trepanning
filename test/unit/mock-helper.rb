require 'test/unit'
require_relative '../../processor/mock'

module MockUnitHelper
  def common_setup(name)
    @dbg, @cmd = MockDebugger::setup(name, false)
  end
end

