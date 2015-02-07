#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../lib/trepanning'

class TestTrepanCallBug < Test::Unit::TestCase

    def define_singleton_method_by_proc(obj, name, block)
        metaclass = class << obj; self; end
        metaclass.send(:define_method, name, block)
    end

    def test_debugger_call_bug
        $calls = []
        mydbgr = nil
        2.times do
            x = 1
            mydbgr = Trepan.new
            core = mydbgr.core
            def core.event_processor_tp(tp)
                frame = tp.frame
                $calls << [frame.source_container, frame.source_location].flatten
            end
            mydbgr.debugger
            y = 2
        end
        mydbgr.stop
        assert_equal true, $calls.size > 0, $calls
    end

end
