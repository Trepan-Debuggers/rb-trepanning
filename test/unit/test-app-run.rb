#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/run'

class TestAppRun < Test::Unit::TestCase
  include Rbdbgr
  def test_basic
    assert_equal(true, File.executable?(whence_file('irb')))
    ng = whence_file('probably-does-not-exist')
    assert_equal(true, File.executable?(ng) || ng == 'probably-does-not-exist')
    rp = ruby_path
    assert_equal(true, File.executable?(rp))
    tup = explicit_restart_argv(ARGV)
    assert_equal(rp, tup[0])
    assert_equal(true, File.readable?(tup[1]))
    assert_equal(ARGV.size + 2, tup.size)
  end

end
