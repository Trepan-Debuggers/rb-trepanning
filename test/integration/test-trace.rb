#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'
require 'rbconfig'

class TestTrace < Test::Unit::TestCase
  @@NAME = File.basename(__FILE__, '.rb')[5..-1]
  TREPAN_LOC =
    if RbConfig::CONFIG['target_os'].start_with?('mingw')
      /.. \((?:[A-Za-z]:)?.+:\d+( @\d+)?\)/
    else
      /.. \(.+:\d+( @\d+)?\)/
    end

    def test_trepan_trace
        opts = {:dbgr => '-x', :args => '3 5', :nocommand => true}
        if RbConfig::CONFIG['target_os'].start_with?('mingw')
            opts[:short_right] = 'trace-mingw'
        end
        opts[:filter] = Proc.new{|got_lines, correct_lines|
            got_lines.each do |line|
                line.gsub!(/\((?:.*\/)?(.+:\d+) @/, '(\1 @') if
                    line =~ TREPAN_LOC
            end
        }
        assert_equal(true, run_debugger(@@NAME, 'gcd.rb', opts))
    end
end
