#!/usr/bin/env ruby
require 'test/unit'
require_relative 'cmd-helper'
require_relative %w(.. .. processor command help)

class TestCommandHelp < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name     = File.basename(__FILE__, '.rb').split(/-/)[2]
    @my_cmd   = @cmds[@name]
  end

  def check_help(should_not_have, *args)
    @cmdproc.instance_variable_set('@msgs', [])
    @cmdproc.instance_variable_set('@errmsgs', [])
    arg_str = args.join(' ')
    @my_cmd.run([@name] + args)
    shoulda = should_not_have ? ['no ', ''] : ['', 'no ']
    msgs = @cmdproc.instance_variable_get('@msgs')
    errmsgs = @cmdproc.instance_variable_get('@errmsgs')
    assert_equal(should_not_have, msgs.empty?,
                 "Expecting %shelp for #{arg_str}.\n Got #{msgs}" %
                 shoulda[0])
    assert_equal(!should_not_have, errmsgs.empty?,
                 "Expecting %serror for #{arg_str}.\n Got #{errmsgs}" %
                 shoulda[1])
  end
  
  def test_help_command

    # Test we can run 'help *cmd* for each command
    @cmds.keys.each do |cmd_name| 
      check_help(false, cmd_name) 
    end

    # Test we can run 'help *alias* for each alias
    @cmdproc.aliases.keys.each do |alias_name| 
      check_help(false, alias_name)
    end

    # double-check specific commands and aliases
    %w(step n help).each do |cmd_pat|
      check_help(false, cmd_pat)
    end

    # Test patterns
    %w(* s.* ste).each do |cmd_pat|
      check_help(false, cmd_pat)
    end

    # Test categories
    %w(running stack).each do |cmd_pat|
      check_help(false, cmd_pat)
    end

    # Test sub help and subhelp patterns
    [%w(info file), %w(show *)].each do |args|
      check_help(false, args)
    end

    # Test invalid commands
    %w(bogus abcd.*).each do |cmd_pat|
      check_help(true, cmd_pat)
    end

    # invalid help subcommands
    [%w(info fdafds), %w(fedafdsa *)].each do |args|
      check_help(true, *args)
    end

    # screwball error
    check_help(true, '["info",')

  end

  # FIXME: Do better than this.
  def check_subcmd_help(cmd_name, subcmd, *args)
    def subcmd.msg(mess)
      @msgs << mess
    end
    subcmd.instance_variable_set('@msgs', [])
    subcmd.instance_variable_set('@errmsgs', [])
    subcmd.help(cmd_name)
    assert subcmd.instance_variable_get('@msgs')
    # subcmd.help
  end

  def test_help_subcommand
    # Get list of commands with subcmds
    cmd_names = @cmds.values.map do |c| 
      c.instance_variable_defined?(:@subcmds) ? c.name : nil
    end.compact
    cmd_names.each do |cmd_name|
      check_subcmd_help(cmd_name, @cmds[cmd_name].subcmds)
    end
  end

end
