#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor/command/base/cmd'

class Trepan
  class Command::Test < Trepan::Command
    NAME = 'test'
    CATEGORY = 'testcategory'
    completion %w(a aa ab ba aac)
  end
end

class TestBaseCommand < Test::Unit::TestCase

  class MockCmdProcessor
    # The below functions aren't tested/called, but they are the
    # methods required by a command and are placeholders for when we
    # do start testing.
    def initialize(dbgr)
    end
    def confirm(message, default)
      p ['confirm: ', message, default]
    end
    def errmsg(message, opts)
      p ['err:', message, opts]
    end
    def msg(message, opts)
      p [message, opts]
    end
    def msg_nocr(message, opts)
      p ['nocr: ', message, opts]
    end
    def section(message, opts)
      p ['section: ', message, opts]
    end
  end

  def setup
    @proc = MockCmdProcessor.new(nil)
    @cmd = Trepan::Command::Test.new(@proc)
  end

  def test_base_completion
    assert_equal(%w(aa aac), @cmd.complete('aa'))
    @cmd.instance_variable_set('@completions', %w(aardvark apple))
    assert_equal(%w(aardvark), @cmd.complete('aa'))
  end
end
