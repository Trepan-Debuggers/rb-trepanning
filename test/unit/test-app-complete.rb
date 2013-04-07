#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/complete'

class TestAppUtil < Test::Unit::TestCase
  include Trepan::Complete
  def test_complete
    hash = {'ab' => 1, 'aac' => 2, 'aa' => 3, 'a' => 4}
    ary = hash.keys.sort
    [[[], 'b'], [ary, 'a'], [%w(aa aac), 'aa'], 
     [ary, ''], [['ab'], 'ab'], [[], 'abc']].each do |result, prefix|
      assert_equal(result, complete_token(ary, prefix),
                   "Trouble matching #{ary}.inspect on #{prefix.inspect}")
    end
    [[ary, 'a'], [%w(aa aac), 'aa'], 
     [['ab'], 'ab'], [[], 'abc']].each do |result_keys, prefix|
      result = result_keys.map {|key| [key, hash[key]]}
      assert_equal(result, complete_token_with_next(hash, prefix),
                   "Trouble matching #{hash}.inspect on #{prefix.inspect}")
    end
    
  end

  def test_next_token
    x = '  now is  the  time'
    [[0, [ 5, 'now']], 
     [2, [ 5, 'now']],
     [5, [ 8, 'is']], 
     [8, [13, 'the']],
     [9, [13, 'the']],
     [13, [19, 'time']],
     [19, [19, '']],
    ].each do |pos, expect|
      assert_equal(expect, next_token(x, pos),
                   "Trouble with next_token(#{x}, #{pos})")

    end
  end

end
