#!/usr/bin/env ruby
require 'test/unit'
require_relative File.join(%w(.. .. lib core))
require_relative File.join(%w(.. .. processor main))
require_relative File.join(%w(.. .. processor command exit))
require_relative File.join(%w(.. .. processor command base_subcmd))

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
  end
  
  def test_base_subcommand
    help_subcmd = DebuggerSubcommand.new(@exit_cmd)
    def help_subcmd.msg(message)
      $msgs << message
    end
    def help_subcmd.errmsg(message)
      $errors << message
    end
    assert help_subcmd
    assert_raises RuntimeError do 
      help_subcmd.run
    end
    assert_equal([], $errors)
    help_subcmd.run_set_int('', 'testing 1 2 3')
    assert_equal(1, $errors.size)
    assert_equal(Fixnum, help_subcmd.settings[:width].class)
  end

end
