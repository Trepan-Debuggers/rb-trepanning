module Rbdbgr
  def valid_condition?(str)
    begin
      RubyVM::InstructionSequence.compile(str)
    rescue SyntaxError
      return nil
    rescue
      nil
    end
  end
end
if __FILE__ == $0
  include Rbdbgr
  p valid_condition?('a+2')
  p valid_condition?('1+')
end
