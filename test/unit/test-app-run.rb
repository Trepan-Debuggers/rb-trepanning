#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/run'

class TestAppRun < Test::Unit::TestCase
  include Trepanning
  def test_whence
    assert_equal(true, File.executable?(whence_file('irb')))
    ng = whence_file('probably-does-not-exist')
    assert_equal(true, File.executable?(ng) || ng == 'probably-does-not-exist')
  end

  def test_ruby_syntax_errors
    assert_nil ruby_syntax_errors __FILE__
    readme = File.join(File.dirname(__FILE__), %w(.. .. README.textile))
    assert ruby_syntax_errors(readme)
  end

end
