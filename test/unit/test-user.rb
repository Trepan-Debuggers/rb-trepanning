#!/usr/bin/env ruby
# Unit test for rbdbgr.interface.user

require 'test/unit'
require_relative %w(.. .. interface user)

# Tests Debugger::UserInterface
class TestInterfaceUser < Test::Unit::TestCase

  # Test interface.user.UserInterface.confirm()
  def test_confirm

    user_intf = Debugger::UserInterface.new
    def user_intf.readline(prompt)
      $response_answer
    end

    ['y', 'Y', 'Yes', '  YES  '].each do |s|
      $response_answer = s
      ans = user_intf.confirm('Testing', true)
      assert_equal(true, ans)
    end
    
    ['n', 'N', 'No', '  NO  '].each do |s|
      $response_answer = s
      ans = user_intf.confirm('Testing', true)
      assert_equal(false, ans)
    end
    # FIXME: Add checking default values. Checking looping 
    # values
    return
  end
  # FIXME: more thorough testing of other routines in user.
end

