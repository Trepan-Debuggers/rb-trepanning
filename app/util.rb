# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
class Trepan
  module Util

  :module_function # All functions below are easily publicly accessible

    def safe_repr(str, max, suffix='...')
      if str.is_a?(String) && str.size > max && !str.index("\n")
        char = str[0..0]
        opt_quote = 
          if '"' == char || "'" == char
            max -= 1
            char
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
  include Trepan::Util
  string = 'The time has come to talk of many things.'
  puts safe_repr(string, 50)
  puts safe_repr(string, 17)
  puts safe_repr(string.inspect, 17)
  puts safe_repr(string.inspect, 17, '')
end
