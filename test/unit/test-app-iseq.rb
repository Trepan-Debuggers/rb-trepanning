#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/file'

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

class TestAppISEQ < Test::Unit::TestCase
  include Trepanning

  def test_find_iseqs
    iseqs = find_iseqs(ISEQS__, "tmpdir")
    assert_equal(false, iseqs.empty?)
    iseqs = find_iseqs(ISEQS__, "tmpdir@#{__FILE__}")
    assert_equal(true, iseqs.empty?)
    iseqs = find_iseqs(ISEQS__, "tmpdir@tmpdir.rb")
    assert_equal(false, iseqs.empty?)
  end

end
