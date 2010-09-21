#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/thread'

class TestAppThread < Test::Unit::TestCase
  include Trepan::ThreadHelper

  def test_basic
    Object::Thread.new do 
      th_main = Thread.main
      th_current = Thread.current
      [[ 0, th_main],   
       [ 1, th_current], 
       [-1, th_current],
       [-2, th_main],
       [2, nil], [-3, nil],
       [Thread.main.object_id, th_main],
       [Thread.current.object_id, th_current]].each do 
         |th_num, expected_th|
         assert_equal(expected_th, get_thread(th_num),
                      "get_thread(#{th_num}) should be expected")
       end
     end.join
  end
end
