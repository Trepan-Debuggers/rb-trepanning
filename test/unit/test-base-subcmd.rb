#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. lib core)
require_relative %w(.. .. processor main)
require_relative %w(.. .. processor command exit)
require_relative %w(.. .. processor command base subcmd)

# Mock debugger stub. FIXME: put in comment helper routine.
class Debugger
end

$errors = []
class TestBaseCommandHelp < Test::Unit::TestCase

  def setup
    $errors   = []
    $msgs     = []
    @dbg      = Debugger.new
    @core     = Debugger::Core.new(@dbg)
    @cmdproc  = @core.processor = Debugger::CmdProcessor.new(@core)
    @cmds     = @cmdproc.instance_variable_get('@commands')
    @exit_cmd = @cmds['exit']
    @exit_subcmd = Debugger::Subcommand.new(@exit_cmd)
    def @exit_subcmd.msg(message)
      $msgs << message
    end
    def @exit_subcmd.errmsg(message)
      $errors << message
    end
  end

  def test_base_subcommand
    assert @exit_subcmd
    assert_raises RuntimeError do 
      @exit_subcmd.run
    end
    assert_equal([], $errors)
    @exit_subcmd.run_set_int('', 'testing 1 2 3')
    assert_equal(1, $errors.size)
    assert_equal(Fixnum, @exit_subcmd.settings[:width].class)
  end

  def test_show_on_off
    assert_equal('on', @exit_subcmd.show_onoff(true))
    assert_equal('off', @exit_subcmd.show_onoff(false))
    assert_equal('unset', @exit_subcmd.show_onoff(nil))
    assert_equal('??', @exit_subcmd.show_onoff(5))
  end
end
