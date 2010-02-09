#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. processor hook)

# Test Debugger:CmdProcessor Hook portion
class TestProcHook < Test::Unit::TestCase

  def test_basic
    @args = []
    hook1 = Proc.new {|name, a| @args << [name, a]}
    hooks = Debugger::CmdProcessor::Hook.new()
    assert_equal(true, hooks.empty?)
    hooks.insert(-1, 'hook1', hook1)
    hooks.run
    assert_equal([['hook1', nil]], @args)
    hooks.insert_if_new(-1, 'hook1', hook1)
    assert_equal([['hook1', nil]], @args)
    
    @args = []
    hooks.insert_if_new(-1, 'hook2', hook1)
    hooks.run(10)
    assert_equal([['hook1', 10], ['hook2', 10]], @args)

    @args = []
    hooks.delete_by_name('hook2')
    hooks.run(30)
    assert_equal([['hook1', 30]], @args)
  end
end
