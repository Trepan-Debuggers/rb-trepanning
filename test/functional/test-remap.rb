#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestRemap < Test::Unit::TestCase

  include FnTestHelper

  def test_remap
    skip "Not sure what this one was supposed to test - Investigate."
    cmds = [
            'step',
            'list',
            ]
    d = strarray_setup(cmds)
    d.start(true)
    ##############################
    require 'date'
    ##############################
    d.stop
    out = [
           'line ',
           "require 'date'",
           'METHOD TestRemap#require(path)',
           '-> ',
           'def require(path) # :doc:',
           ' 20    	  #   is.',
           ' 21    	  # * Otherwise, installed gems are searched for a file that matches.',
           " 22    	  #   If it's found in gem 'y', that gem is activated (added to the",
           ' 23    	  #   loadpath).',
           ' 24    	  #',
           ' 25    	  # The normal <tt>require</tt> functionality of returning false if',
           ' 26    	  # that file has already been loaded is preserved.',
           ' 27    	',
           ' 28  ->	  def require(path) # :doc:',
           ' 29    	    gem_original_require path',
           'line ',
           'gem_original_require path',
           '<- ',
           'R=> false',
           'end',
           'line ',
           'd.stop'
          ]
    compare_output(out, d, cmds)

  end

end
