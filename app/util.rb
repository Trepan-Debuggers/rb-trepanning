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

    # extract the "expression" part of a line of source code.
    # 
    def extract_expression(text)
      if text =~ /^\s*(?:if|elsif|unless)\s+/
        text.gsub!(/^\s*(?:if|elsif|unless)\s+/,'') 
        text.gsub!(/\s+then\s*$/, '')
      elsif text =~ /^\s*(?:until|while)\s+/
        text.gsub!(/^\s*(?:until|while)\s+/,'') 
        text.gsub!(/\s+do\s*$/, '')
      elsif text =~ /^\s*return\s+/
        # EXPRESION in: return EXPRESSION
        text.gsub!(/^\s*return\s+/,'')
      elsif text =~ /^\s*case\s+/
        # EXPRESSION in: case EXPESSION
        text.gsub!(/^\s*case\s*/,'')
      elsif text =~ /^\s*def\s*.*\(.+\)/
        text.gsub!(/^\s*def\s*.*\((.*)\)/,'[\1]')
      elsif text =~ /^\s*[A-Za-z_][A-Za-z0-9_\[\]]*\s*=[^=>]/
        # RHS of an assignment statement.
        text.gsub!(/^\s*[A-Za-z_][A-Za-z0-9_\[\]]*\s*=/,'')
      end
      return text
    end
  end
end

if __FILE__ == $0
  include Trepan::Util
  # save repr
  string = 'The time has come to talk of many things.'
  puts safe_repr(string, 50)
  puts safe_repr(string, 17)
  puts safe_repr(string.inspect, 17)
  puts safe_repr(string.inspect, 17, '')
  # ------------------------------------
  # extract_expression
  ['if condition("if")', 
   'until until_termination',
   'return return_value',
    'nothing_to_be.done'
  ].each do |stmt|
    puts extract_expression stmt
  end
end
