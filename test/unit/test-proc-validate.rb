#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. processor validate)

$errors = []
$msgs   = []

# Test Debugger:CmdProcessor Frame portion
class TestValidate < Test::Unit::TestCase

  def setup
    $errors = []
    $msgs   = []

    if Debugger::CmdProcessor.instance_methods.member?(:load_debugger_commands)
      # Rake test does this
      @proc = Debugger::CmdProcessor.new(MockDebugger.new())
    else
      # Ruby run on this file only does this
      @proc = Debugger::CmdProcessor.new
    end
    class << @proc
      def errmsg(msg)
        $errors << msg
      end
      def print_location
        # $msgs << "#{@frame.source_container} #{@frame.source_location[0]}"
        $msgs << "#{@frame.source_container} "
        # puts $msgs
      end
    end
  end

  # See that we have can load up commands
  def test_basic
    onoff = 
    [['1', true],  ['on', true],
     ['0', false], ['off', false]].each do |arg, expected|
      assert_equal(expected, @proc.get_onoff(arg))
    end
    [['1', 1],  ['1E', nil], ['bad', nil], ['1+1', 2], ['-5', -5]].each do 
      |arg, expected|
      assert_equal(expected, @proc.get_int_noerr(arg))
    end
  end
end
