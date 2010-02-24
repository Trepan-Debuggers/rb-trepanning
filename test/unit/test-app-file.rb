#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. app file)

if defined?(SCRIPT_ISEQS__) && SCRIPT_ISEQS__.is_a?(Hash)
  SCRIPT_ISEQS__.clear
else
  SCRIPT_ISEQS__ = {} 
end
if defined?(ISEQS__) && ISEQS__.is_a?(Hash)
  ISEQS__.clear
else  
  ISEQS__ = {}
end

# To have something to work with.
load 'tmpdir.rb'

class TestAppFile < Test::Unit::TestCase
  include Rbdbgr

  def test_file_match_pat
    assert_equal('(?:^|[/])abc\.rb$', file_match_pat('abc.rb'))
    assert_equal('^/a/abc\.rb$', file_match_pat('/a/abc.rb'))
  end

  def test_find_iseqs
    iseqs = find_iseqs(ISEQS__, "tmpdir")
    assert_equal(false, iseqs.empty?)
    iseqs = find_iseqs(ISEQS__, "tmpdir@#{__FILE__}")
    assert_equal(true, iseqs.empty?)
    iseqs = find_iseqs(ISEQS__, "tmpdir@tmpdir.rb")
    assert_equal(false, iseqs.empty?)
  end

  def test_find_scripts
    [['tmpdir.rb', true], ['/tmpdir.rb', false],
     ['sometmpdir.rb', false]].each do 
      |filename, expected|
      found = find_scripts(filename)
      assert_equal(expected, !found.empty?,
                   "Should %shave found %s; got %s" % 
                   [expected ? '' : 'not ', filename, found.inspect])
    end
  end
end
