#!/usr/bin/env ruby
require_relative 'cmd-helper'

# Test Debugger:CmdProcessor Location portion
class TestProcLocation < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name = File.basename(__FILE__, '.rb').split(/-/)[2]
    @cmdproc.settings[:basename] = true
  end

  def test_it
    assert_equal File.basename(__FILE__), @cmdproc.canonic_file(__FILE__)
    eval <<-EOE
      @cmdproc.frame_initialize
      @cmdproc.frame_setup(RubyVM::ThreadFrame.current)
      assert @cmdproc.current_source_text
    EOE
  end
end
