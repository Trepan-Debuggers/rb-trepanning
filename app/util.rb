class Debugger
  module Util

  :module_function # All functions below are easily publically accessible

    def safe_repr(str, max, suffix='...')
      if str.is_a?(String) && str.size > max && !str.index("\n")
        opt_quote = 
          if '"' == str[0] || "'" == str[0] 
            max -= 1
            str[0]
          else
            ''
          end
        "%s%s%s" % [ str[0...max], opt_quote, suffix ]
      else
        str
      end
    end
    
  end
end

if __FILE__ == $0
  include Debugger::Util
  string = 'The time has come to talk of many things.'
  puts safe_repr(string, 50)
  puts safe_repr(string, 17)
  puts safe_repr(string.inspect, 17)
  puts safe_repr(string.inspect, 17, '')
end
