# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
# A CodeRay Scanner for YARV
# FIXME: add unit test.
require 'rubygems'
require 'coderay'
module CodeRay
  module Scanners
    
    class YARV < Scanner
      
      include Streamable
      
      register_for :yarv
      file_extension 'yarv'

      def string_parse(tokens)
        if match = scan(/^"[^"]*"/)
          string = match.dup
          while  "\\" == match[-2..-2]
            match = scan(/^.*"/)
            break unless match 
            string << match 
          end
          tokens << [string, :string]
          true
        else
          false
        end
      end

      def space_parse(tokens)
        if match = scan(/^[ \t]*/)
          tokens << [match, :space] if match
        end
      end

      def scan_tokens(tokens, options)

        state = :initial
        number_expected = true

        until eos?

          kind = nil
          match = nil
          
          case state

          when :initial
            if match = scan(/^\s*/)
              tokens << [match, :space]  unless match.empty?
            end
            state = 
              if match = scan(/^$/)
                :initial
              else
                :expect_label
              end

          when :expect_label
            state = 
              if match = scan(/\d+/x)
                tokens << [match, :label]
                :expect_opcode
              else 
                match = scan(/^.*$/)
                tokens << [match, :error]
                :initial
            end
            
          when :expect_opcode
            state = 
              if match = scan(/(\s+)(\S+)/)
                match =~ /(\s+)(\S+)/                
                tokens << [$1, :space]
                opcode = $2
                tokens << [opcode, :reserved]
                :expect_operand
              else
                match = scan(/^(.*)$/)
                tokens << [match, :error]
                :initial
              end

          when :expect_literal
            space_parse(tokens)
            if match = scan(/^#<.+>/)
              tokens << [match, :content]
            elsif match = scan(/^(\d+)/)
              tokens << [match, :integer]
            elsif match = scan(/^([:][^: ,\n]+)/)
              tokens << [match, :symbol]
            elsif string_parse(tokens)
              # 
            elsif match = scan(/nil|true|false/)
              tokens << [match, :pre_constant]
            elsif match = scan(/\/.*\//)
              tokens << [match, :entity]
            else
              match = scan(/^.*$/)              
              tokens << [match, :error] unless match.empty?
              end
            state = :expect_opt_lineno
            
          when :expect_operand
            space_parse(tokens)
            state = :expect_another_operand
            if match = scan(/^(\d+)/)
              tokens << [match, :integer]
            elsif match = scan(/\/.*\//)
              # Really a regular expression
              tokens << [match, :entity]
            elsif match = scan(/^([:][^: ,\n]+)/)
              tokens << [match, :symbol]
            elsif match = scan(/^([$][^ ,\n]+)/)
              tokens << [match, :global_variable]
            elsif match = scan(/nil|true|false/)
              tokens << [match, :pre_constant]
            elsif match = scan(/^<.+>/)
              tokens << [match, :content]
            elsif string_parse(tokens)
            else
              state = :expect_opt_lineno
            end
            
          when :expect_another_operand
            state = 
              if match = scan(/^,/)
                tokens << [match, :operator]
                :expect_operand
              else
                :expect_opt_lineno
              end
          when :expect_opt_lineno
            space_parse(tokens)
            if match = scan(/^\(\s+\d+\)$/)
              tokens << [match, :comment] 
            else
              match = scan(/^.*$/)              
              tokens << [match, :error] unless match.empty?
            end
            state = :initial
          end
        end
        tokens
      end
    end
  end
end
if __FILE__ == $0
  require 'term/ansicolor'
  ruby_scanner = CodeRay.scanner :yarv
string='
     0045 opt_regexpmatch1 /^-e$/
     0000 trace            1                                              (   9)
     0002 putnil           
     0003 putstring        "irb"
     0005 send             :require, 1, nil, 8, <ic:0>
     0011 pop              
     0012 trace            1                                              (  11)
     0014 putstring        "/usr/local/bin/irb"
     0016 getglobal        $0
     0018 opt_eq           <ic:8>
     0020 branchunless     41
     0022 trace            1                                              (  12)
     0024 getinlinecache   31, <ic:2>
     0027 getconstant      :IRB
     0029 setinlinecache   <ic:2>
     0031 putstring        "/usr/local/bin/irb"
     0033 send             :start, 1, nil, 0, <ic:3>
'  
  yarv_scanner = CodeRay.scanner :yarv
  tokens = yarv_scanner.tokenize(string)
  p tokens
  yarv_highlighter = CodeRay::Duo[:yarv, :term]
  puts yarv_highlighter.encode(string)
end
