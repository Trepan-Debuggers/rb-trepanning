#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../lib/rbdbgr'

class TestDebuggerCallBug < Test::Unit::TestCase

  def test_debugger_call_bug
    $calls = []
    mydbgr = nil
    2.times do
      x = 1
      mydbgr = Debugger.new()
      mydbgr.core.processor.define_singleton_method(:process_commands) do
        |frame|
        # p [frame.source_container, frame.source_location, @core.event].flatten
        $calls << [frame.source_container, frame.source_location].flatten
      end
      mydbgr.debugger
      y = 2
    end
    mydbgr.stop  
    assert_equal true, $calls.size > 0, $calls
  end
  
end






