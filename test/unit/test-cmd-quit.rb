#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor/mock'
require_relative '../../processor/command/quit'

class TestCommandQuit < Test::Unit::TestCase

  def setup
    @name      = File.basename(__FILE__, '.rb').split(/-/)[2]
    @dbg, @cmd = MockDebugger::setup(@name, false)
  end

  def test_basic
    pid = fork { @cmd.run([@name, '10']) }
    Process.wait
    assert_equal(10, $?.exitstatus)
    pid = fork { @cmd.run([@name]) }
    Process.wait
    assert_equal(0, $?.exitstatus)
    # FIXME: should test that finalization routines get run;
    # should test 'confirm' gets run; and should test that
    # 'unconditional' is handled correctly.
  end
  

end
