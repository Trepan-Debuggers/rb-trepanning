#!/usr/bin/env ruby
require 'test/unit'
require 'rbconfig'
require_relative '../../app/run'

class TestAppRun < Test::Unit::TestCase
  include Trepanning
  def test_whence
    resolved_file = whence_file('irb')
    if RbConfig::CONFIG['target_os'].start_with?('mingw')
      assert_equal(true, File.readable?(resolved_file),
                   "file #{resolved_file} should be readable")
    else
      assert_equal(true, File.executable?(resolved_file),
                   "file #{resolved_file} should be executable")
    end
    ng = whence_file('probably-does-not-exist')
    assert_equal(true, File.executable?(ng) || ng == 'probably-does-not-exist')
  end

  def test_ruby_syntax_errors
    assert_nil ruby_syntax_errors __FILE__
    readme = File.join(File.dirname(__FILE__), %w(.. .. README.textile))
    assert ruby_syntax_errors(readme)
  end

end
