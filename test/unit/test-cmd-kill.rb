#!/usr/bin/env ruby
require 'test/unit'
require_relative %w(.. .. lib core)
require_relative %w(.. .. processor main)
require_relative %w(.. .. processor command help)

# Mock debugger stub. FIXME: put in comment helper routine.
class Debugger
end

class TestCommandKill < Test::Unit::TestCase

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
    def @cmdproc.errmsg(message)
      @errmsgs << message
    end
    reset_cmdproc_vars
  end

  def reset_cmdproc_vars
    @cmdproc.instance_variable_set('@msgs', [])
    @cmdproc.instance_variable_set('@errmsgs', [])
    @cmdproc.leave_cmd_loop = false
  end
  
  def no_test_kill_command
    @my_cmd.run([@name, 'foo'])
    assert_equal(false,  @cmdproc.leave_cmd_loop)

    save_trap = Signal.trap(10) {
      @success = true
    }

    @success = false
    @my_cmd.run([@name, '10'])
    assert_equal(true,  @success, 
                 'Should have run the handler')

  ensure
    # restore old trap if any
    trap(10, save_trap) if save_trap
  end

end
