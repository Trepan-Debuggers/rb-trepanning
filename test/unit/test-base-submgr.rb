#!/usr/bin/env ruby
require_relative 'cmd-helper'
# We will use set as our canonical example
require_relative '../../processor/command/set'

class TestBaseSubcommandMgr < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
  end

  def test_basic
    my_cmd = @cmds['show']
    # require 'trepanning'; debugger
    assert_equal([:debug, :different], my_cmd.complete('d'),
                 "Should be able to complete 'd'")
    assert_equal(%w(show args), my_cmd.subcmds.lookup('ar').prefix,
                 "Should be able to complete lookup('ar')")
    assert_equal(nil, my_cmd.subcmds.lookup('a'), 
                 "Shouldn't find since we have 'show args' and 'show auto'")
  end
end
