# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>

class Trepan
  module Util

    module_function 
    def safe_repr(str, max, elipsis='... ')
      if str.is_a?(String) && str.size > max && !str.index("\n")
        "%s%s%s" % [ str[0...max/2], elipsis,  str[str.size-max/2..str.size]]
      else
        str
      end
    end

    def complete_token(complete_ary, prefix)
      complete_ary.select { |cmd| cmd.to_s.start_with?(prefix) }.sort
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
