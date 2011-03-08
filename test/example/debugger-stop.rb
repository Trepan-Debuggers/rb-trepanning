# !/usr/bin/env ruby
# Bug in debugger stopping where we were inside debugger was stopping
# inside itself because multiple debugger names in the presence of modules
# 
require_relative '../../lib/trepanning'
module Foo
  module_function
  def five
    debugger  # Resolves to Module#debugger not Kernel#debugger
    5
  end
end
debugger(:nx=>true)
Foo::five
