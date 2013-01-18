#!/usr/bin/env ruby
require_relative './mock-helper'
require_relative './cmd-helper'
require_relative '../../processor/command/edit'

class TestCommandEdit < Test::Unit::TestCase
  include MockUnitHelper
  def setup
    @name = File.basename(__FILE__, '.rb').split(/-/)[2]
    $msgs = []
    $errmsgs = []
    common_setup(@name)
    def @cmd.msg(message)
      $msgs << message
    end
    def @cmd.errmsg(message)
      $errmsgs << message
    end
  end

  def test_basic
    editor = ENV['EDITOR'] || '/bin/ex'
    skip "Can't find editor to use" unless File.executable?(editor)
    old_editor = ENV['EDITOR']
    ENV['EDITOR'] = '#'
    base_file = File.basename(__FILE__)
    @cmd.proc.settings[:basename] = true
    @cmd.run([@name])
    assert_equal "Running # +39 \"edit.rb\"...", $msgs[-1]
    @cmd.run([@name, 7])
    assert_equal "Running # +7 \"edit.rb\"...", $msgs[-1]
    @cmd.run([@name, __FILE__])
    assert_equal "Running # +1 \"#{base_file}\"...", $msgs[-1]
  end
end
