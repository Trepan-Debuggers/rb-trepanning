#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor/command/base/subsubcmd'

$errors = []
class TestBaseSubCommand < Test::Unit::TestCase

  def test_prefix_set
    Trepanning::SubSubcommand.set_name_prefix('/tmp/show_subcmd/auto_subcmd/food.rb', 
                                           self.class)
    assert_equal('food', NAME, "should have set NAME")
    assert_equal('show auto food', CMD)
    assert_equal(%w(show auto food), PREFIX)
  end

end
