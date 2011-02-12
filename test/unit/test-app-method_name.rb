#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/method_name'

class TestAppMethodName < Test::Unit::TestCase

  module Testing
    def testing; 5 end
    module_function :testing
  end

  def test_parse_identifier
    %w(a a1 $global __FILE__ Constant).each do |name|
      begin
        match = MethodName.parse(name, :root => :identifier)
        assert_equal name, match.to_s, "should be able to parse of #{name}"
      rescue Citrus::ParseError
        assert false, "should be able to parse of #{name}"
      end
    end
    %w(0 1e10 a.b).each do |name|
      begin
        match = MethodName.parse(name, :root => :identifier)
        assert false, "should not have been able to parse of #{name}"
      rescue Citrus::ParseError
        assert true, "should not have been able to parse of #{name}"
      end
    end
  end

  def test_parse_method
    [['Object', 0], ['A::B', 1], ['A::B::C', 2],
     ['A::B::C::D', 3], ['A::B.c', 2], ['A.b.c.d', 3]].each do |name, count|
      begin
        match = MethodName.parse(name, :root => :class_module_chain)
        assert_equal match.to_s, name, "should be able to parse of #{name}"
        m = match.value.chain[1]
        count.times do |i|
          assert m, "Chain item #{i} of #{name} should not be nil"
          m = m.value.chain[1]
        end
        assert_nil m, "#{count} chain item in #{match} should be nil"
      rescue Citrus::ParseError
        assert false, "should be able to parse of #{name}"
      end
    end
    ['A(5)'].each do |name|
      begin
        match = MethodName.parse(name, :root => :class_module_chain)
        p match
        assert false, "should not have been able to parse of #{name}"
      rescue Citrus::ParseError
        assert true, "should not have been able to parse of #{name}"
      end
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
    %w(Array.map).each do |str|
      meth = meth_for_string(str, binding)
      assert meth.kind_of?(UnboundMethod), "#{meth.class}"
  end
    %w(O5).each do |str|
      begin 
        meth = meth_for_string(str, binding)
        assert false, "should have found a method for #{str}"
      rescue NameError
        assert_nil meth, "Should not have found a method for #{str}"
      end
    end
  end
end
