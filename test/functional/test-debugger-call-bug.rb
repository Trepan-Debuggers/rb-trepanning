#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../lib/trepanning'

class TestTrepanCallBug < Test::Unit::TestCase

  def test_debugger_call_bug
      skip "FIXME"
    $calls = []
    mydbgr = nil
    2.times do
      x = 1
      mydbgr = Trepan.new()
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
