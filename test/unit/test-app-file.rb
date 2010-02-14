#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. app file)

SCRIPT_ISEQS__ = {} unless 
  defined?(SCRIPT_ISEQS__) && SCRIPT_ISEQS__.is_a?(Hash)
ISEQS__        = {} unless 
    defined?(ISEQS__) && ISEQS__.is_a?(Hash)

class TestLibBrkpt < Test::Unit::TestCase

  def test_basic
    load 'tmpdir.rb'

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
