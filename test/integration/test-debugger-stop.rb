#!/usr/bin/env ruby
require 'test/unit'
require 'rbconfig'
require_relative 'helper'

class TestDebuggerStop < Test::Unit::TestCase
  @@name = File.basename(__FILE__, '.rb')[5..-1]
  @@name = @@name[2..-1] if
    (RbConfig::CONFIG['target_os'].start_with?('mingw') and
     @@name =~ /^[A-Za-z]:/)

  def test_it
    skip "FIXME for mingw" if
      RbConfig::CONFIG['target_os'].start_with?('mingw')
    opts = {}
    opts[:feed_input] = "echo 'info program ;; continue ;; quit!' "
    opts[:filter] = Proc.new{|got_lines, correct_lines|
      got_lines[0].gsub!(/\(.*debugger-stop.rb[:]\d+ @\d+/,
                         'debugger-stop.rb:14 @1955')
      # require_relative '../../lib/trepanning'; debugger
      got_lines[2].gsub!(/\(.*debugger-stop.rb[:]\d+ @\d+\)/,
                         'debugger-stop.rb:10 @1954')
      got_lines[4].gsub!(/PC offset \d+ .*<top .+debugger-stop.rb/,
                         "PC offset 100 <top debugger-stop.rb")
    }
    assert_equal(true, run_debugger(@@name, @@name + '.rb', opts))
  end
end
