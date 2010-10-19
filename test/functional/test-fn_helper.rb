#!/usr/bin/env ruby
require 'test/unit'
require_relative 'fn_helper'

class TestFnTestHelper < Test::Unit::TestCase

  include FnTestHelper

  def test_basic
    assert_equal(__LINE__, get_lineno, 'get_lineno()')
    assert_equal(0,
                 '-- (/tmp/trepan/tmp/gcd.rb:4)' =~ TREPAN_LOC)
    assert_equal(0, '(trepan): exit' =~ TREPAN_PROMPT)

    output='
-- (/tmp/trepan/tmp/gcd.rb:4)
(trepan): s
-- (/tmp/trepan/tmp/gcd.rb:18)
(trepan): s
-- (/tmp/trepan/tmp/gcd.rb:19)
(trepan): s
.. (/tmp/trepan/tmp/gcd.rb:0)
(trepan): s
-> (/tmp/trepan/tmp/gcd.rb:4)
'.split(/\n/)
   expect='
-- (/tmp/trepan/tmp/gcd.rb:4)
-- (/tmp/trepan/tmp/gcd.rb:18)
-- (/tmp/trepan/tmp/gcd.rb:19)
.. (/tmp/trepan/tmp/gcd.rb:0)
-> (/tmp/trepan/tmp/gcd.rb:4)
'.split(/\n/)
  assert_equal(expect, filter_line_cmd(output))
  end
  
end






