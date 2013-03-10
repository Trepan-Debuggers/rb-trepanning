#!/usr/bin/env ruby
require 'test/unit'

# Test Trepan::CmdProcessor::Default
class TestProcDefault < Test::Unit::TestCase

  DIR = File.dirname(__FILE__)
  DEFAULT_RBFILE = File.join(%W(#{DIR} .. .. processor default.rb))

  def test_maxwidth_setting
    ENV['COLUMNS'] = '50'
    Trepan::CmdProcessor.send(:remove_const, 'DEFAULT_SETTINGS') if
      Trepan::CmdProcessor.const_defined?('DEFAULT_SETTINGS')
    load DEFAULT_RBFILE
    assert_equal(50, Trepan::CmdProcessor::DEFAULT_SETTINGS[:maxwidth],
                 'Pick up COLUMNS environment')

    Trepan::CmdProcessor.send(:remove_const, 'DEFAULT_SETTINGS')
    load DEFAULT_RBFILE

    ENV['COLUMNS'] = '0'
    assert_operator(Trepan::CmdProcessor::DEFAULT_SETTINGS[:maxwidth],
                    :>=, 10,
                 'Change too small COLUMNS environment value')

    Trepan::CmdProcessor.send(:remove_const, 'DEFAULT_SETTINGS')
    load DEFAULT_RBFILE

    ENV['COLUMNS'] = 'non-numeric-string'
    assert_operator(Trepan::CmdProcessor::DEFAULT_SETTINGS[:maxwidth],
                    :>=, 10,
                 'Change too small COLUMNS environment value')
  end
end
