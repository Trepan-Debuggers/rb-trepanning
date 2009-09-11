#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. lib core)
require_relative %w(.. .. processor main)
require_relative %w(.. .. processor command help)

# Mock debugger stub. FIXME: put in comment helper routine.
class Debugger
end

class TestCommandHelp < Test::Unit::TestCase

  def setup
    @dbg      = Debugger.new
    @core     = Debugger::Core.new(@dbg)
    @cmdproc  = @core.processor = Debugger::CmdProcessor.new(@core)
    @cmds     = @cmdproc.commands
    @name     = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd   = @cmds[@name]
    def @cmdproc.msg(message)
      @msgs << message
    end
  end
  
  # Test we can run 'help *cmd* for each command
  def test_help_command
    @cmds.each do |cmd_name, cmd|
      @cmdproc.instance_variable_set('@msgs', [])
      @my_cmd.run([@name, cmd_name])
      assert_equal(false,  @cmdproc.instance_variable_get('@msgs').empty?)
    end
  end

end
