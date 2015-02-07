#!/usr/bin/env ruby
require_relative 'cmd-helper'

# Test Debugger:CmdProcessor location portion
class TestCmdProcessorLocation < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name ||= File.basename(__FILE__, '.rb').split(/-/)[2]
    @cmdproc.settings[:basename] = true
  end

  def test_canonic_file
    @cmdproc.settings[:basename] = false
    assert_equal File.expand_path(__FILE__), @cmdproc.canonic_file(__FILE__)
    @cmdproc.settings[:basename] = true
    assert_equal File.basename(__FILE__), @cmdproc.canonic_file(__FILE__)
  end

  def test_eval_get_source_text
    assert_equal File.basename(__FILE__), @cmdproc.canonic_file(__FILE__)
    eval <<-EOE
      @cmdproc.frame_initialize
      @cmdproc.frame_setup(RubyVM::Frame.get)
      assert @cmdproc.current_source_text
    EOE
  end
end
