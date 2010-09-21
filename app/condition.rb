# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
class Trepan
  module Condition
    def valid_condition?(str)
      begin
        RubyVM::InstructionSequence.compile(str)
      rescue SyntaxError
        return nil
      rescue
        nil
      end
    end
    module_function :valid_condition?
  end
end
if __FILE__ == $0
  include Trepan::Condition
  p valid_condition?('a+2')
  puts '-' * 20
  p valid_condition?('1+')
  puts '-' * 20
end
