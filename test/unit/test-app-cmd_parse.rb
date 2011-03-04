#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/cmd_parse'

class TestCmdParse < Test::Unit::TestCase

  def no__test_parse_location
    ['fn', 'fn 5', 'fn @5', '@5', '5', 'fn:5', 'fn:@5'].each do |location|
      begin
        match = MethodName.parse(location, :root => :location)
        assert_equal location, match.to_s, "should be able to parse of #{location}"
      rescue Citrus::ParseError
        assert false, "should be able to parse of #{location}"
      end
    end
    
    # Check actual values
    [['@5', 5, :vm_offset], ['10', 10, :line_number]].each do 
      |position, expect, symbol|
      match = MethodName.parse(position, :root => symbol)
      assert_equal(expect, match.value.value, 
                   "should be able to get value of location #{position}")
    end
    # %w(0 1e10 a.b).each do |location|
    #   begin
    #     match = MethodName.parse(location, :root => :identifier)
    #     assert false, "should not have been able to parse of #{location}"
    #   rescue Citrus::ParseError
    #     assert true, "should not have been able to parse of #{location}"
    #   end
    # end
  end
  
  module Testing
    def testing; 5 end
    module_function :testing
  end

  def test_parse_identifier
    %w(a a1 $global __FILE__ Constant).each do |name|
      cp = CmdParse.new(name)
      assert cp._identifier, "should be able to parse of #{name}"
    end
    %w(0 1e10 @10).each do |name|
      cp = CmdParse.new(name)
      assert_equal(true, !cp._identifier, 
                   "should not have been able to parse of #{name}")
    end
  end

  def test_parse_method
    [['Object', 0], ['A::B', 1], ['A::B::C', 2],
     ['A::B::C::D', 3], ['A::B.c', 2], ['A.b.c.d', 3]].each do |name, count|
      cp = CmdParse.new(name)
      assert cp._class_module_chain, "should be able to parse of #{name}"
      m = cp.result
      count.times do |i|
        assert m, "Chain item #{i} of #{name} should not be nil"
        m = m.chain[1]
      end
      assert_nil m.chain, "#{count} chain item in #{cp.result} should be nil"
    end
    ['A(5)'].each do |name|
      cp = CmdParse.new(name)
      cp._class_module_chain
      assert_not_equal(name, cp.result.name,
                   "should not have been able to parse of #{name}")
    end
  end

  include Trepan::CmdParser
  def test_method_name
    def five; 5 end
    %w(five RubyVM::InstructionSequence.new Kernel.eval
        Testing.testing Kernel::eval File.basename).each do |str|
      meth = meth_for_string(str, binding)
      assert meth.kind_of?(Method), "#{str} method's class should be Method, not #{meth.class}"
    end
    x = File
    def x.five; 5; end
    %w(x.basename x.five).each do |str|
      meth = meth_for_string(str, binding)
      assert meth.kind_of?(Method), "#{str} method's class should be Method, not #{meth.class}"
    end
    %w(Array.map).each do |str|
      meth = meth_for_string(str, binding)
      assert meth.kind_of?(UnboundMethod), "#{meth.class}"
    end
    %w(O5).each do |str|
      meth = meth_for_string(str, binding)
      assert_equal nil, meth, "should have found a method for #{str}"
    end
  end
end
