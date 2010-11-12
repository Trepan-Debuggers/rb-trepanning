#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/run'

class TestAppRun < Test::Unit::TestCase
  include Trepanning
  def test_basic
    assert_equal(true, File.executable?(whence_file('irb')))
    ng = whence_file('probably-does-not-exist')
    assert_equal(true, File.executable?(ng) || ng == 'probably-does-not-exist')
    rp = ruby_path
    assert_equal(true, File.executable?(rp))
  end

end
