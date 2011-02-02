#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor/main'
require_relative '../../app/mock'

class TestCmdProcessorLoadCmds < Test::Unit::TestCase

  def setup
    @proc = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    @proc.instance_variable_set('@settings', {})
  end

  # See that we have can load up commands
  def test_basic
    @proc.load_cmds_initialize
    assert_equal(false, @proc.commands.empty?)
    assert_equal(false, @proc.aliases.empty?)
  end

  def test_complete
    assert_equal(%w(debug delete directory disable disassemble display down),
                 @proc.complete("d"),
                 "Failed completion of 'd' commands")
    assert_equal(['show debug', 'show different'], @proc.complete("sho d"),
                 "Failed completion of 'sho d' subcommands")
    $errors = []
  end

  def test_run_cmd
    $errors = []

    def @proc.errmsg(mess)
      $errors << mess
    end

    def test_it(size, *args)
      @proc.run_cmd(*args)
      assert_equal(size, $errors.size, $errors)
    end
    test_it(1, 'foo')
    test_it(2, [])
    test_it(3, ['list', 5])
    # See that we got different error messages
    assert_not_equal($errors[0], $errors[1], $errors)
    assert_not_equal($errors[1], $errors[2], $errors)
    assert_not_equal($errors[2], $errors[0], $errors)
  end

end
