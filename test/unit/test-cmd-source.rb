#!/usr/bin/env ruby
require_relative './mock-helper'
require_relative '../../processor/command/source'

# Test command source
class TestCommandSource < Test::Unit::TestCase

  include MockUnitHelper
  def setup
    @name = File.basename(__FILE__, '.rb').split(/-/)[2]
    common_setup(@name)
  end

  def test_source_opts

  [['--quiet',       :verbose, false],
   ['-q',            :quiet, true],
   ['--no-quiet',    :quiet, false],
   ['--continue',    :abort_on_error, false],
   ['--no-continue', :abort_on_error, true],
   ['-c',            :abort_on_error, false],
   ['-v',            :verbose, true],
   ['--verbose',     :verbose, true],
   ['--no-verbose',  :verbose, false]
  ].each do |opt, key, expect|
      options = 
        @cmd.parse_options(Trepan::Command::SourceCommand::DEFAULT_OPTIONS.dup,
                           opt)
      assert_equal(expect, options[key], 
                   "Option #{opt} using key #{key} failure")
    end
  end
end
