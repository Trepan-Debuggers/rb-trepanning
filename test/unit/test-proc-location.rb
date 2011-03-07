#!/usr/bin/env ruby
require 'test/unit'
require 'thread_frame'
require_relative '../../processor/main' # Have to include before frame!
                                        # FIXME
require_relative '../../processor/frame'
require_relative '../../app/mock'

$errors = []
$msgs   = []

# Test Debugger:CmdProcessor Location portion
class TestProcLocation < Test::Unit::TestCase

  def setup
    $errors = []
    $msgs   = []
    @proc    = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    @proc.instance_variable_set('@settings', {:basename => true})
    @proc.frame_index = 0
    @proc.frame_initialize
    @proc.location_initialize
    class << @proc
      def errmsg(msg)
        $errors << msg
      end
    end
  end

  def test_it
    assert_equal File.basename(__FILE__), @proc.canonic_file(__FILE__)
    eval <<-EOE
      @proc.frame_initialize
      @proc.frame_setup(RubyVM::ThreadFrame.current)
      @proc.location_initialize
      assert @proc.current_source_text
    EOE
  end
end
