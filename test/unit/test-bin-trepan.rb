#!/usr/bin/env ruby
require 'test/unit'
require 'rbconfig'
load File.join(File.dirname(__FILE__), %w(.. .. bin trepan))

# Test bin/trepan Module methods
class TestBinTrepan < Test::Unit::TestCase

  include Trepanning

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
