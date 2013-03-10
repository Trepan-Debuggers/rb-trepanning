#!/usr/bin/env ruby
require_relative 'cmd-helper'
require_relative '../../processor/command/alias'
require_relative '../../processor/command/unalias'

# Test commands alias and unalias
class TestCommandAliasUnalias < Test::Unit::TestCase

  include UnitHelper
  def setup
    common_setup
    @name     = File.basename(__FILE__, '.rb').split(/-/)[2]
  end

  def check_alias(should_not_have, cmd_name, *args)
    @cmdproc.instance_variable_set('@msgs', [])
    @cmdproc.instance_variable_set('@errmsgs', [])
    arg_str = args.join(' ')
    my_cmd   = @cmds[cmd_name]
    my_cmd.run([cmd_name] + args)
    shoulda = should_not_have ? ['no ', ''] : ['', 'no ']
    msgs = @cmdproc.instance_variable_get('@msgs')
    errmsgs = @cmdproc.instance_variable_get('@errmsgs')
    assert_equal(should_not_have, msgs.empty?,
                 "Expecting %s%s for #{arg_str}.\n Got #{msgs}" %
                 [shoulda[0], cmd_name])
    assert_equal(!should_not_have, errmsgs.empty?,
                 "Expecting %serror for #{arg_str}.\n Got #{errmsgs}" %
                 shoulda[1])
  end

  def alias_defined?(alias_name)
    @cmdproc.aliases.member?(alias_name)
  end
  
  def test_alias_unalias_command

    assert_equal(false, @cmdproc.aliases.empty?,
                 'There should be some aliases defined')

    assert_equal(false, alias_defined?('ki'))
    check_alias(false, 'alias', 'ki', 'kill')
    assert_equal(true, alias_defined?('ki'))
    check_alias(false, 'unalias', 'ki')
    assert_equal(false, alias_defined?('ki'))

  end

end
