#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../processor/main'
require_relative '../../app/core'

# Mock debugger stub. FIXME: put in comment helper routine.
class Debugger
end

# Test Debugger:CmdProcessor
class TestCmdProcessor < Test::Unit::TestCase

  def setup
    @dbg     = Debugger.new
    @core    = Debugger::Core.new(@dbg)
    @cmdproc = @core.processor = Debugger::CmdProcessor.new(@core)
    @cmds    = @cmdproc.instance_variable_get('@commands')
  end

  # See that we have can load up commands
  def test_command_load
    assert @cmds.is_a?(Hash), 'Should have gotten a command hash'
    assert @cmds.keys.size > 0, 'Should have found at least one command'
    cmd_name, cmd_obj = @cmds.first
    assert cmd_name.is_a?(String), 'Should have string name for a command'
    assert(cmd_obj.kind_of?(Debugger::Command), 
           'Command should be a Debugger:Command')
    assert cmd_obj.respond_to?(:run), 'Command should have a run method'
  end

  # See that each command has required constants defined.  Possibly in
  # a strongly-typed language we wouldn't need to do much of this.
  def test_command_class_vars
    @cmds.each do |cmd_name, cmd|
      %w(HELP CATEGORY NAME SHORT_HELP).each do
        |const|
        value = cmd.class.const_get(const)
        assert(value.is_a?(String),
               "#{const} in command #{cmd_name} should be a String; got #{value}.")
      end
      %w(MIN_ARGS MAX_ARGS).each do
        |const|
        value = cmd.class.const_get(const)
        assert(value.is_a?(Fixnum) || value.nil?,
               "#{const} in command #{cmd_name} should be a Fixnum or nil; got #{value}.")
      end
      %w(ALIASES).each do
        |const|
        next unless cmd.class.constants.member?(:ALIASES)
        ary = cmd.class.const_get(const)
        assert(ary.is_a?(Array),
               "#{const} in command #{cmd_name} should be an Array")

        ary.each do |v| 
          assert(v.is_a?(String),
                 "#{const} in command #{cmd_name} should be Array of Strings; got #{v}.")
        end
      end
    end
  end

  def test_run_command
    def run_and_check(cmd, expect_msgs, expect_errmsgs, add_msg)
      $errs = []; $msgs = []
      @cmdproc.run_command(cmd)
      assert_equal(expect_msgs, $msgs, 
                   "#{add_msg}: mismatched messages from #{caller[0]}")
      assert_equal(expect_errmsgs, $errs, 
                   "Mismatched error messages from #{caller[0]}")
    end
    def @cmdproc.msg(mess)
      $msgs << "#{mess}"
    end
    def @cmdproc.errmsg(mess)
      $errs << "#{mess}"
    end
    run_and_check('!s=1', ['R=> 1'], [], "! evaluation")
    run_and_check('print "foo"', ['foo'], [], "print command")
    run_and_check('set autoeval off', 
                  ['Evaluation of unrecognized debugger commands is off.'], [], 
                  "autoeval set")
    run_and_check('asdf', [], 
                  ['Undefined command: "asdf". Try "help".'], 
                  "invalid command")
    
  end

end
