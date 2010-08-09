#!/usr/bin/env ruby
require 'test/unit'
require 'rbconfig'
load File.join(File.dirname(__FILE__), %w(.. .. bin rbdbgr))

# Test bin/rbdbgr Module methods
class TestBinRbdbgr < Test::Unit::TestCase

  include Rbdbgr

  def test_ruby_path
    rb_path = ruby_path
    assert_equal(true, File.executable?(rb_path),
                 "#{rb_path} should be an executable Ruby interpreter")

    # Let us test that we get *exactly* the same configuration as we
    # have in this. I'm a ball buster.
    cmd = "#{rb_path} -rrbconfig -e 'puts Marshal.dump(RbConfig::CONFIG)'"
    rb_config = Marshal.load(`#{cmd}`)
    assert_equal(RbConfig::CONFIG, rb_config,
                 "#{rb_path} config doesn't match got:
#{rb_config.pretty_inspect}
expected: 
#{RbConfig::CONFIG.pretty_inspect}
")
  end

  def test_whence_file
    abs_path_me = File.expand_path(__FILE__)
    assert_equal(abs_path_me, whence_file(abs_path_me),
                 "whence_file should have just returned #{abs_path_me}")

    basename_me = File.basename(__FILE__)
    dirname_me  = File.dirname(__FILE__)

    # Add my directory onto the beginning of PATH
    path_dirs = ENV['PATH'].split(File::PATH_SEPARATOR)
    path_dirs.unshift(dirname_me)
    ENV['PATH'] = path_dirs.join(File::PATH_SEPARATOR)

    assert_equal(File.join(dirname_me, basename_me), 
                 whence_file(basename_me),
                 "whence_file should have found me")
    # Restore old path
    path_dirs.shift 
    ENV['PATH'] = path_dirs.join(File::PATH_SEPARATOR)
  end
end
