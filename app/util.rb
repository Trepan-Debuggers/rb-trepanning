module Rbdbgr

  :module_function # All functions below are easily publically accessible

  def safe_repr(str, max)
    if str.is_a?(String) && str.size > max && !str.index("\n")
      str[0...max] + '...' 
    else
      str
    end
  end

end

if __FILE__ == $0
  include Rbdbgr
  string = 'The time has come to talk of many things.'
  puts safe_repr(string, 50)
  puts safe_repr(string, 17)
end
