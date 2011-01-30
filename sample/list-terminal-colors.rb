#!/usr/bin/env ruby
# Run this program in a terminal window to see a list of terminal colors
# the assignments are the defaults as of Jan 18, 2011, but you may want to 
# double check.
require 'rubygems'
require 'coderay'
require 'term/ansicolor'

      TOKEN_COLORS = {
        :annotation => '35',
        :attribute_name => '33',
        :attribute_name_fat => '33',
        :attribute_value => '31',
        :attribute_value_fat => '31',
        :bin => '1;35',
        :char => {:self => '36', :delimiter => '34'},
        :class => '1;35',
        :class_variable => '36',
        :color => '32',
        :comment => '37',
        :complex => '34',
        :constant => ['34', '4'],
        :decoration => '35',
        :definition => '1;32',
        :directive => ['32', '4'],
        :doc => '46',
        :doctype => '1;30',
        :doc_string => ['31', '4'],
        :entity => '33',
        :error => ['1;33', '41'],
        :exception => '1;31',
        :float => '1;35',
        :function => '1;34',
        :global_variable => '42',
        :hex => '1;36',
        :important => '1;31',
        :include => '33',
        :integer => '1;34',
        :interpreted => '1;35',
        :key => '35',
        :label => '1;4',
        :local_variable => '33',
        :oct => '1;35',
        :operator_name => '1;29',
        :pre_constant => '1;36',
        :pre_type => '1;30',
        :predefined => ['4', '1;34'],
        :preprocessor => '36',
        :pseudo_class => '34',
        :regexp => {
          :content => '31',
          :delimiter => '1;29',
          :modifier => '35',
          :function => '1;29'
        },
        :reserved => '1;31',
        :shell => {
          :self => '42',
          :content => '1;29',
          :delimiter => '37',
        },
        :string => {
          :self => '32',
          :modifier => '1;32',
          :escape => '1;36',
          :delimiter => '1;32',
        },
        :symbol => '1;32',
        :tag => '34',
        :tag_fat => '1;34',
        :tag_special => ['34', '4'],
        :type => '1;34',
        :value => '36',
        :variable => '34',
        :insert => '42',
        :delete => '41',
        :change => '44',
        :head => '45',
      }
      TOKEN_COLORS[:keyword] = TOKEN_COLORS[:reserved]
      TOKEN_COLORS[:method] = TOKEN_COLORS[:function]
      TOKEN_COLORS[:imaginary] = TOKEN_COLORS[:complex]
      TOKEN_COLORS[:open] = TOKEN_COLORS[:close] = TOKEN_COLORS[:nesting_delimiter] = TOKEN_COLORS[:escape] = TOKEN_COLORS[:delimiter]

# require 'trepanning'; debugger
%w(annotation
   attribute_name
   attribute_name_fat
   attribute_value
   attribute_value_fat
   bin
   class
   class_variable
   color
   comment
   complex
   constant
   decoration
   definition
   doc
   doctype
   entity
   exception
   float
   function
   global_variable
   hex
   important
   include
   integer
   interpreted
   key
   label
   local_variable
   oct
   operator_name
   pre_constant
   pre_type
   predefined
   preprocessor
   pseudo_class
   reserved
   symbol
   tag
   tag_fat
   type
   value
   variable
   insert
   delete
   change
   head
).each do |name|
     puts "\e[#{TOKEN_COLORS[name.to_sym]}m#{name} - #{TOKEN_COLORS[name.to_sym].inspect}\e[0m"
end
%w(bold italic underline).each do |name|
     attrib=Term::ANSIColor.send(name)
     puts "#{attrib}#{name} - #{attrib.inspect}\e[0m"
end
